#include <string.h>
#include <Python.h>
#include <Rinternals.h>

#if PY_MAJOR_VERSION >= 3
#define PyString_Check PyBytes_Check
#define PyString_AsString PyBytes_AsString
#define PyStr2UTF8 PyUnicode_AsUTF8
#else
#define PyStr2UTF8 PyString_AsString
#endif

static PyObject *CHAR2Py(SEXP sWhat) {
    const char *utf8 = translateCharUTF8(sWhat);
    PyObject *o = PyUnicode_DecodeUTF8(utf8, strlen(utf8), 0);
    if (!o)
	Rf_error("invalid string - cannot be converted to valid UTF-8");
    return o;
}

static char *pyhome_buf, *pyprog_buf;
static PyObject *main_dict;
static int py_inited = 0;

#define is_pyref(X) inherits((X), "pyref")
#define is_valid_pyref(X) (inherits((X), "pyref") && R_ExternalPtrAddr(X))

static void free_pyref(SEXP sRef) {
    if (is_pyref(sRef)) {
	PyObject *o = (PyObject*) R_ExternalPtrAddr(sRef);
	if (o) Py_DECREF(o);
    }	
}

SEXP rpy_fetch_ex();

static SEXP rpy_last_exception;

SEXP rpy_last_ex() { return rpy_last_exception ? rpy_last_exception : R_NilValue; }

static PyObject *unwrap_py(SEXP s) {
    if (!is_pyref(s))
        Rf_error("invalid python reference object");
    return (PyObject*) R_ExternalPtrAddr(s);
}

static void chk_ex(PyObject *o) {
    if (!o) { /* automatic exception mapping - rudimentary for now */
	if (rpy_last_exception && rpy_last_exception != R_NilValue)
	    R_ReleaseObject(rpy_last_exception);
	rpy_last_exception = rpy_fetch_ex();
	if (rpy_last_exception != R_NilValue) {
	    const char *desc = "";
	    SEXP val;
	    R_PreserveObject(rpy_last_exception);
	    if ((val = getAttrib(rpy_last_exception, install("value"))) != R_NilValue)
		desc = PyStr2UTF8(PyObject_Str(unwrap_py(val)));
	    Rf_error("Python exception: %s %s", PyStr2UTF8(PyObject_Str(unwrap_py(rpy_last_exception))), desc);
	}
    }
}

static SEXP wrap_py(PyObject *o) {
    SEXP res;
    /* FIXME: should we map None to NULL automatically? 
       if (o == Py_None) return R_NilValue; */
    chk_ex(o);
    res = PROTECT(R_MakeExternalPtr(o, R_NilValue, R_NilValue));
    if (o) Py_INCREF(o);
    R_RegisterCFinalizerEx(res, free_pyref, TRUE);
    setAttrib(res, install("class"), mkString("pyref"));
    UNPROTECT(1);
    return res;
}

SEXP rpy_init(SEXP sPyHome, SEXP sPyProgram) {
    if (py_inited)
	return ScalarLogical(FALSE);
    if (TYPEOF(sPyHome) == STRSXP && LENGTH(sPyHome) > 0) {
	pyhome_buf = strdup(CHAR(STRING_ELT(sPyHome, 0)));
	Py_SetPythonHome(pyhome_buf);
    }
    if (TYPEOF(sPyProgram) == STRSXP && LENGTH(sPyProgram) > 0) {
	pyprog_buf = strdup(CHAR(STRING_ELT(sPyProgram, 0)));
	Py_SetProgramName(pyprog_buf);
    }
    /* init - don't let python mess with signals */
    Py_InitializeEx(0);
    py_inited = 1;
    return ScalarLogical(TRUE);
}

static void chk_init() {
    if (!py_inited) {
	Rf_warning("Python is not initialized, calling py.init() implicitly");
	rpy_init(R_NilValue, R_NilValue);
    }
}

/* get (possibly cached) main dict
   NOTE: this can raise R errors - unlikely, but possible */
static PyObject *get_main_dict() {
    if (!main_dict) { /* we're caching __main__ since it's most often used */
	PyObject *main_mod  = PyImport_AddModule("__main__");
	if (!main_mod) Rf_error("cannot get __main__ module");
	if (!(main_dict = PyModule_GetDict(main_mod)))
	    Rf_error("cannot get __main__ dictionary");
	/* this should not be needed since it shoudl live forever, but to make sure */
	Py_INCREF(main_dict);
    }
    return main_dict;
}

static PyObject *get_module_dict(SEXP sModule) {
    if (is_pyref(sModule)) {
	PyObject *o = unwrap_py(sModule);
	if (o && PyDict_Check(o))
	    return o;
	if (o && PyModule_Check(o)) {
	    o = PyModule_GetDict(o);
	    chk_ex(o);
	    return o;
	}
	/* FIXME: do we need to hold on the dict ref? We could simply wrap it if we have to. */
    } else if (sModule == R_NilValue)
	return get_main_dict();
    else if (TYPEOF(sModule) == STRSXP && LENGTH(sModule) == 1) {
	PyObject *o = PyImport_ImportModule(CHAR(STRING_ELT(sModule, 0)));
	chk_ex(o);
	if (o && PyModule_Check(o)) {
	    o = PyModule_GetDict(o);
	    chk_ex(o);
	    return o;
	}
    }

    Rf_error("invalid module");
    return 0; /* not reached */
}

static SEXP unwrap_rcaps(PyObject *o) {
    const char *name = PyCapsule_GetName(o); /* you *have* to get the name to get the pointer -
						this is somewhat stupid but we didn't invent the API ;) */
    SEXP s = (SEXP) PyCapsule_GetPointer(o, name);
    if (!s)
	Rf_error("Invalid NULL pointer R reference on the Python side");
    return s;
}

static SEXP py2CHAR(PyObject *o) {
    if (PyUnicode_Check(o)) {
	SEXP res;
#if PY_MAJOR_VERSION < 3
	/* in 2.x it has to go from Uni to UTF8 bytes to C */
	o = PyUnicode_AsUTF8String(o);
	res = mkCharCE(PyString_AsString(o), CE_UTF8);
	Py_DECREF(o);
#else
	/* in 3.3+ we have PyUnicode_AsUTF8 */
	Py_ssize_t s_size = 0;
	return mkCharLenCE(PyUnicode_AsUTF8AndSize(o, &s_size), s_size, CE_UTF8);
#endif
	return res;
    }
    if (PyString_Check(o))
	return mkChar(PyString_AsString(o));
    /* FIXME: this can in theory be infinite recursion,
       but it should never be the case if PyObject_Str
       does what it's documented to do ... */
    return py2CHAR(PyObject_Str(o));
}

static int is_homogeneous_list(PyObject *o) {
    int i, n = PyList_Size(o);
    PyTypeObject *type;
    if (n < 2) return 1; /* by definition */
    type = PyList_GetItem(o, 0)->ob_type;
    for (i = 1; i < n; i++)
	if (PyList_GetItem(o, i)->ob_type != type)
	    return 0;
    return 1;
}

static int is_homogeneous_tuple(PyObject *o) {
    int i, n = PyTuple_Size(o);
    PyTypeObject *type;
    if (n < 2) return 1; /* by definition */
    type = PyTuple_GetItem(o, 0)->ob_type;
    for (i = 1; i < n; i++)
	if (PyTuple_GetItem(o, i)->ob_type != type)
	    return 0;
    return 1;
}

static SEXP py_scalar(PyObject *o) {
    if (o == Py_None)
	return R_NilValue;
    if (PyUnicode_Check(o)) {
	SEXP res;
#if PY_MAJOR_VERSION < 3
        /* in 2.x it has to go from Uni to UTF8 bytes to C */
        o = PyUnicode_AsUTF8String(o);
        res = mkCharCE(PyString_AsString(o), CE_UTF8);
        Py_DECREF(o);
#else
        /* in 3.3+ we have PyUnicode_AsUTF8 */
        Py_ssize_t s_size = 0;
        return mkCharLenCE(PyUnicode_AsUTF8AndSize(o, &s_size), s_size, CE_UTF8);
#endif
	return res;
    }
    if (PyString_Check(o))
	return mkString(PyString_AsString(o));
    if (PyBool_Check(o))
	return ScalarLogical(PyInt_AS_LONG(o) ? TRUE : FALSE);
    if (PyInt_Check(o))
	return ScalarInteger(PyInt_AS_LONG(o)); /* FIXME: switch to double for >32-bit ints */
    if (PyFloat_Check(o))
	return ScalarReal(PyFloat_AsDouble(o));
    /* FIXME: COMPLEX/ PyComplex_Check PyComplex_RealAsDouble PyComplex_ImagAsDouble */
    return 0;
}

/* simplify - very crude for now */
static SEXP py_to_sexp(PyObject *o) {
    SEXP res;
    chk_ex(o);
    if (o == Py_None)
	return R_NilValue;
    if (PyCapsule_CheckExact(o))
	return unwrap_rcaps(o);
    if ((res = py_scalar(o))) return res;

    /* both tuples and lists are mapped to generic vectors since there is no difference in R */
    if (PyTuple_Check(o)) {
	int n = PyTuple_GET_SIZE(o), i;
	SEXP res;
	if (!n) return allocVector(VECSXP, 0);
	if (n > 1 && is_homogeneous_tuple(o)) {
	    PyObject *e0 = PyTuple_GetItem(o, 0);
	    PyTypeObject *type = e0->ob_type;
	    int i;
	    if (type == &PyInt_Type) {
		if (PyBool_Check(e0)) { /* bool is a subset of int */
		    int *v;
		    res = allocVector(LGLSXP, n);
		    v = (int*) LOGICAL(res);
		    for (i = 0; i < n; i++)
			v[i] = PyInt_AS_LONG(PyTuple_GetItem(o, i));
		    return res;
		} else {
		    int *v;
		    res = allocVector(INTSXP, n);
		    v = (int*) INTEGER(res);
		    for (i = 0; i < n; i++)
			v[i] = PyInt_AS_LONG(PyTuple_GetItem(o, i));
		    return res;
		}
	    } else if (type == &PyLong_Type) {
		double *v;
		res = allocVector(REALSXP, n);
		v = REAL(res);
		for (i = 0; i < n; i++)
		    v[i] = PyLong_AsDouble(PyTuple_GetItem(o, i));
		return res;
	    } else if (type == &PyFloat_Type) {
		double *v;
		res = allocVector(REALSXP, n);
		v = REAL(res);
		for (i = 0; i < n; i++)
		    v[i] = PyFloat_AS_DOUBLE(PyTuple_GetItem(o, i));
		return res;
	    } else if (type == &PyUnicode_Type || type == &PyString_Type) {
		res = PROTECT(allocVector(STRSXP, n));
		for (i = 0; i < n; i++)
		    SET_STRING_ELT(res, i, py2CHAR(PyTuple_GetItem(o, i)));
		UNPROTECT(1);
		return res;
	    } /* FIXME: bytes in py3? */
	} /* homogeneous */

	res = PROTECT(allocVector(VECSXP, n));
	for (i = 0; i < n; i++)
	    SET_VECTOR_ELT(res, i, py_to_sexp(PyTuple_GetItem(o, i)));
	UNPROTECT(1);
	return res;
    }
    if (PyList_Check(o)) {
	int n = PyList_GET_SIZE(o), i;
	SEXP res;
	if (!n) return allocVector(VECSXP, 0);
	if (n > 1 && is_homogeneous_list(o)) {
	    PyObject *e0 = PyList_GetItem(o, 0);
	    PyTypeObject *type = e0->ob_type;
	    int i;
	    if (type == &PyInt_Type) {
		if (PyBool_Check(e0)) { /* bool is a subset of int */
		    int *v;
		    res = allocVector(LGLSXP, n);
		    v = (int*) LOGICAL(res);
		    for (i = 0; i < n; i++)
			v[i] = PyInt_AS_LONG(PyList_GetItem(o, i));
		    return res;
		} else {
		    int *v;
		    res = allocVector(INTSXP, n);
		    v = (int*) INTEGER(res);
		    for (i = 0; i < n; i++)
			v[i] = PyInt_AS_LONG(PyList_GetItem(o, i));
		    return res;
		}
	    } else if (type == &PyLong_Type) {
		double *v;
		res = allocVector(REALSXP, n);
		v = REAL(res);
		for (i = 0; i < n; i++)
		    v[i] = PyLong_AsDouble(PyList_GetItem(o, i));
		return res;
	    } else if (type == &PyFloat_Type) {
		double *v;
		res = allocVector(REALSXP, n);
		v = REAL(res);
		for (i = 0; i < n; i++)
		    v[i] = PyFloat_AS_DOUBLE(PyList_GetItem(o, i));
		return res;
	    } else if (type == &PyUnicode_Type || type == &PyString_Type) {
		res = PROTECT(allocVector(STRSXP, n));
		for (i = 0; i < n; i++)
		    SET_STRING_ELT(res, i, py2CHAR(PyList_GetItem(o, i)));
		UNPROTECT(1);
		return res;
	    } /* FIXME: PyBytes in py3 ? */
	} /* homogeneous */
	
	res = PROTECT(allocVector(VECSXP, n));
	for (i = 0; i < n; i++)
	    SET_VECTOR_ELT(res, i, py_to_sexp(PyList_GetItem(o, i)));
	UNPROTECT(1);
	return res;
    }

    /* we map dicts to lists - we could also use envs instead that may be closer in spirit... */
    if (PyDict_Check(o)) {
	int n = PyDict_Size(o), i;
	/* FIXME: those will leak in case of an R error */
	PyObject *keys = PyDict_Keys(o), *vals = PyDict_Values(o);
	SEXP nam, res = PROTECT(py_to_sexp(vals));
	Py_DECREF(vals);
	setAttrib(res, R_NamesSymbol, nam = allocVector(STRSXP, n));
	for (i = 0; i < n; i++)
	    SET_STRING_ELT(nam, i, py2CHAR(PyList_GetItem(keys, i)));
	Py_DECREF(keys);
	UNPROTECT(1);
	return res;
    }
    /* callable objects */
    if (PyFunction_Check(o) || PyObject_HasAttrString(o, "__call__")) {
	/* FIXME: we shoudl eval in our namespace */
	SEXP fargs = PROTECT(list1(R_MissingArg)), res;
	SEXP py_call = PROTECT(lang3(install("py.call"), wrap_py(o), R_DotsSymbol));
	SEXP call = PROTECT(lang3(install("function"), fargs, py_call));
	SET_TAG(fargs, R_DotsSymbol);
	res = eval(call, R_GlobalEnv);
	UNPROTECT(3);
	return res;
    }
    return wrap_py(o);
}

SEXP rpy_eval(SEXP sWhat, SEXP sGlobals, SEXP sLocals, SEXP sFail) {
    int i, n;
    int fail = (asInteger(sFail) == 1) ? 1 : 0;
    PyObject *globals, *locals, *res = 0;
    if (TYPEOF(sWhat) != STRSXP)
	Rf_error("invalid code - must be a character vector");
    chk_init();
    n = LENGTH(sWhat);
    if (!n)
	return R_NilValue;
    globals = get_module_dict(sGlobals);
    locals = get_module_dict(sLocals);
    for (i = 0; i < n; i++) {
	res = PyRun_String(CHAR(STRING_ELT(sWhat, i)), Py_file_input, globals, locals);
	if (!res) {
	    if (fail)
		chk_ex(res);
	    /* Rf_error("PyRun failed at '%s'", CHAR(STRING_ELT(sWhat, i))); */
	    return R_NilValue;
	}
    }
    return py_to_sexp(res);
}

SEXP rpy_import(SEXP sName) {
    PyObject *o;
    if (TYPEOF(sName) != STRSXP || LENGTH(sName) != 1)
	Rf_error("invalid module name, must be a string");
    chk_init();
    o = PyImport_ImportModule(CHAR(STRING_ELT(sName, 0)));
    return wrap_py(o);
}

SEXP rpy_get(SEXP sName, SEXP sModule, SEXP sFail, SEXP sRef) {
    int as_ref = (asInteger(sRef) == 1) ? 1 : 0;
    int fail = (asInteger(sFail) == 1) ? 1 : 0;
    PyObject *res, *dict = 0;
    const char *name;
    if (TYPEOF(sName) != STRSXP || LENGTH(sName) != 1)
	Rf_error("invalid variable name");
    name = CHAR(STRING_ELT(sName, 0));
    chk_init();
    dict = get_module_dict(sModule);
    if (!(res = PyDict_GetItemString(dict, name))) {
	if (fail) {
	    chk_ex(res);
	    Rf_error("cannot find Python variable `%s'", name);
	}
	return R_NilValue;
    }
    return as_ref ? wrap_py(res) : py_to_sexp(res);
}

SEXP rpy_get_attr(SEXP sObj, SEXP sName, SEXP sRef) {
    int as_ref = (asInteger(sRef) == 1) ? 1 : 0;
    PyObject *o = unwrap_py(sObj);
    if (TYPEOF(sName) != STRSXP || LENGTH(sName) != 1)
	Rf_error("invalid attribute name - must be a string");
    o = PyObject_GetAttrString(o, CHAR(STRING_ELT(sName, 0)));
    return as_ref ? wrap_py(o) : py_to_sexp(o);
}

static PyObject *mk_py_str2list(SEXP sWhat) {
    int i, n = LENGTH(sWhat);
    PyObject *o = PyList_New(n);
    for (i = 0; i < n; i++)
	PyList_SetItem(o, i, CHAR2Py(STRING_ELT(sWhat, i)));
    return o;
}

/* convert R objects to Pyton object */
static PyObject *to_pyref(SEXP sWhat) {
    SEXP res;
    if (sWhat == R_NilValue)
	return Py_None;
    /* pass-through references */
    if (is_pyref(sWhat))
	return unwrap_py(sWhat);
    /* handle some primitive types */
    /* FIXME: should we make this conditional on the object bit? */
    if (TYPEOF(sWhat) == STRSXP) {
	SEXP sNames;
	int i, n = LENGTH(sWhat);
	PyObject *o;
	if ((sNames = getAttrib(sWhat, R_NamesSymbol)) != R_NilValue) {
	    if (LENGTH(sNames) != LENGTH(sWhat))
		Rf_error("names mismatch");
	    o = PyDict_New();
	    for (i = 0; i < n; i++)
		PyDict_SetItemString(o, CHAR(STRING_ELT(sNames, i)), CHAR2Py(STRING_ELT(sWhat, i)));
	    return o;
	} else {
	    PyObject *o;
	    /* python doesn't have proper vector support so we have to worry about scalars ...*/
	    if (n == 1)
		return CHAR2Py(STRING_ELT(sWhat, 0));
	    o = PyTuple_New(n);
	    for (i = 0; i < n; i++)
		PyTuple_SetItem(o, i, CHAR2Py(STRING_ELT(sWhat, i)));
	    return o;
	}
    }
    if (TYPEOF(sWhat) == REALSXP) {
	SEXP sNames;
	int i, n = LENGTH(sWhat);
	PyObject *o;
	double *d = REAL(sWhat);
	if ((sNames = getAttrib(sWhat, R_NamesSymbol)) != R_NilValue) {
	    if (LENGTH(sNames) != LENGTH(sWhat))
		Rf_error("names mismatch");
	    o = PyDict_New();
	    for (i = 0; i < n; i++)
		PyDict_SetItemString(o, CHAR(STRING_ELT(sNames, i)), PyFloat_FromDouble(d[i]));
	    return o;
	} else {
	    PyObject *o;
	    /* python doesn't have proper vector support so we have to worry about scalars ...*/
	    if (n == 1)
		return PyFloat_FromDouble(d[0]);
	    o = PyTuple_New(n);
	    for (i = 0; i < n; i++)
		PyTuple_SetItem(o, i, PyFloat_FromDouble(d[i]));
	    return o;
	}
    }
    if (TYPEOF(sWhat) == INTSXP) {
	SEXP sNames;
	int i, n = LENGTH(sWhat);
	PyObject *o;
	int *v = INTEGER(sWhat);
	if ((sNames = getAttrib(sWhat, R_NamesSymbol)) != R_NilValue) {
	    if (LENGTH(sNames) != LENGTH(sWhat))
		Rf_error("names mismatch");
	    o = PyDict_New();
	    for (i = 0; i < n; i++) /* FIXME: what do we do with NAs? */
		PyDict_SetItemString(o, CHAR(STRING_ELT(sNames, i)), PyInt_FromLong(v[i]));
	    return o;
	} else {
	    PyObject *o;
	    /* python doesn't have proper vector support so we have to worry about scalars ...*/
	    if (n == 1)
		return PyInt_FromLong(v[0]);
	    o = PyTuple_New(n);
	    for (i = 0; i < n; i++)
		PyTuple_SetItem(o, i, PyInt_FromLong(v[i]));
	    return o;
	}
    }
    if (TYPEOF(sWhat) == LGLSXP) {
	SEXP sNames;
	int i, n = LENGTH(sWhat);
	PyObject *o;
	int *v = LOGICAL(sWhat);
	if ((sNames = getAttrib(sWhat, R_NamesSymbol)) != R_NilValue) {
	    if (LENGTH(sNames) != LENGTH(sWhat))
		Rf_error("names mismatch");
	    o = PyDict_New();
	    for (i = 0; i < n; i++) /* FIXME: what do we do with NAs? */
		PyDict_SetItemString(o, CHAR(STRING_ELT(sNames, i)), PyBool_FromLong(v[i]));
	    return o;
	} else {
	    PyObject *o;
	    /* python doesn't have proper vector support so we have to worry about scalars ...*/
	    if (n == 1)
		return PyBool_FromLong(v[0]);
	    o = PyTuple_New(n);
	    for (i = 0; i < n; i++)
		PyTuple_SetItem(o, i, PyBool_FromLong(v[i]));
	    return o;
	}
    }
    if (TYPEOF(sWhat) == VECSXP) {
	SEXP sNames;
	int i, n = LENGTH(sWhat);
	PyObject *o;	
	if ((sNames = getAttrib(sWhat, R_NamesSymbol)) != R_NilValue) {
	    if (LENGTH(sNames) != LENGTH(sWhat))
		Rf_error("names mismatch");
	    o = PyDict_New();
	    for (i = 0; i < n; i++) /* FIXME: what do we do with NAs? */
		PyDict_SetItemString(o, CHAR(STRING_ELT(sNames, i)), to_pyref(VECTOR_ELT(sWhat, i)));
	    return o;
	} else {
	    o = PyTuple_New(n);
	    for (i = 0; i < n; i++)
		PyTuple_SetItem(o, i, to_pyref(VECTOR_ELT(sWhat, i)));
	    return o;
	}
    }
    /* call as.pyref() method */
    res = Rf_eval(lang2(install("as.pyref"), sWhat), R_GlobalEnv);
    if (is_pyref(sWhat))
	return unwrap_py(sWhat);
    Rf_error("failed as.pyref() conversion to Python");
    return 0;
}

SEXP rpy_set(SEXP sName, SEXP sValue, SEXP sModule, SEXP sFail) {
    int fail = (asInteger(sFail) == 1) ? 1 : 0;
    PyObject *dict = 0, *value;
    const char *name;
    if (TYPEOF(sName) != STRSXP || LENGTH(sName) != 1)
	Rf_error("invalid variable name");
    name = CHAR(STRING_ELT(sName, 0));
    chk_init();
    dict = get_module_dict(sModule);
    value = to_pyref(sValue);
    if (PyDict_SetItemString(dict, name, value)) {
	if (fail) {
	    chk_ex(0);
	    Rf_error("cannot set Python variable `%s'", name);
	}
	return ScalarLogical(FALSE);
    }
    return ScalarLogical(TRUE);
}

SEXP rpy_set_attr(SEXP sObj, SEXP sName, SEXP sValue) {
    PyObject *o = unwrap_py(sObj), *value;
    if (TYPEOF(sName) != STRSXP || LENGTH(sName) != 1)
	Rf_error("invalid attribute name - must be a string");
    value = to_pyref(sValue);
    if (PyObject_SetAttrString(o, CHAR(STRING_ELT(sName, 0)), value))
	chk_ex(0);
    return sObj;
}

SEXP rpy_call(SEXP sArgs) {
    SEXP p, sCall;
    int arg_named = 0, arg_unnamed = 0, i = 0, as_ref = 0;
    PyObject *co, *args, *dict = 0;
    p = CDR(sArgs);
    if (p == R_NilValue)
	Rf_error("missing callable object");
    sCall = CAR(p);
    if (!is_pyref(sCall))
	Rf_error("callable object is not a Python reference");
    co = unwrap_py(sCall);
    if (!co || !PyCallable_Check(co))
	Rf_error("trying to call non-callable object in Python");
    p = CDR(p);
    while (p != R_NilValue) {
	const char *tag;
	if (TAG(p) == R_NilValue) arg_unnamed++;
	else if ((tag = CHAR(PRINTNAME(TAG(p))))[0] == '.') {
	    if (!strcmp(tag, ".ref"))
		as_ref = (asInteger(CAR(p)) == 1) ? 1 : 0;
	} else arg_named++; /* we reserve .XX for our internal args */
	p = CDR(p);
    }
    args = PyTuple_New(arg_unnamed);
    if (arg_named)
	dict = PyDict_New();
    p = CDDR(sArgs);
    while (p != R_NilValue) {
	if (TAG(p) != R_NilValue && CHAR(PRINTNAME(TAG(p)))[0] == '.')
	    {} /* ignore internal args */
	else {
	    PyObject *o = to_pyref(CAR(p));
	    if (TAG(p) != R_NilValue)
		PyDict_SetItemString(dict, CHAR(PRINTNAME(TAG(p))), o);
	    else
		PyTuple_SetItem(args, i++, o);
	}
	p = CDR(p);	    
    }
    return as_ref ? wrap_py(PyObject_Call(co, args, dict)) : py_to_sexp(PyObject_Call(co, args, dict));
}

SEXP rpy_fetch_ex() {
    PyObject *ex_type, *ex_value, *ex_traceback;
    PyErr_Fetch(&ex_type, &ex_value, &ex_traceback);
    if (ex_type) {
	SEXP ex = PROTECT(wrap_py(ex_type));
	if (ex_value) {
	    setAttrib(ex, install("value"), wrap_py(ex_value));
	    Py_DECREF(ex_value);
	}
	if (ex_traceback) {
	    setAttrib(ex, install("traceback"), wrap_py(ex_traceback));
	    Py_DECREF(ex_traceback);
	}
	Py_DECREF(ex_type);
	UNPROTECT(1);
	return ex;
    }
    return R_NilValue;
}

static void rcaps_free(PyObject *o) {
    /* we don't use unwrap becasue we don't want to raise any errors
       and we'll get to free the name anyway */
    char *name = (char*) PyCapsule_GetName(o);
    SEXP s = (SEXP) PyCapsule_GetPointer(o, name);
    if (s)
	R_ReleaseObject(s);
    free(name);
}

/* FIXME: it would be nice if we could use the same ref objects as rpy2
   to allow inter-operability */
static PyObject *wrap_rcaps(SEXP sWhat, SEXP sName) {
    const char *name = 0;
    PyObject *cap;
    if (TYPEOF(sName) == STRSXP && LENGTH(sName) > 0)
	name = strdup(CHAR(STRING_ELT(sName, 0)));
    cap = PyCapsule_New(sWhat, name, rcaps_free);
    chk_ex(cap);
    return cap;
}

SEXP rpy_rcaps(SEXP sWhat, SEXP sName) {
    return wrap_py(wrap_rcaps(sWhat, sName));
}

SEXP rpy_as_string(SEXP sRef) {
    PyObject *o;
    if (!is_valid_pyref(sRef)) Rf_error("invalid python reference");
    o = unwrap_py(sRef);
    if (!PyString_Check(o) && !PyUnicode_Check(o)) { /* call str() on the object */
	SEXP res;
	o = PyObject_Str(o);
	res = ScalarString(py2CHAR(o));
	Py_DECREF(o);
	return res;
    }
    return ScalarString(py2CHAR(o));
}

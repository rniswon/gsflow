/*+
 * United States Geological Survey
 *
 * PROJECT  : Modular Modeling System (MMS)
 * NAME     : declparam.c
 * AUTHOR   : CADSWES
 * DATE     : 
 * FUNCTION :
 * COMMENT  : initializes a module variable entry in the memory database
 *
 *      There are 2 functions: declparam() to be called from C
 *                             declparam_() to be called from Fortran
 *
 *      Returns 0 if successful, 1 otherwise.
 * REF      :
 * REVIEW   :
 * PR NRS   :
 *
 * $Id: declparam.c 6045 2011-10-03 19:46:59Z markstro $
 *
   $Revision: 6045 $
        $Log: declparam.c,v $
        Revision 1.21  2001/05/04 20:58:22  markstro
        Added the xml print file

        Revision 1.20  1996/09/10 16:25:22  markstro
        Unknown

 * Revision 1.19  1996/02/19  19:59:50  markstro
 * Now lints pretty clean
 *
        Revision 1.18  1995/06/08 18:01:49  markstro
        (1)  Fixed info window
        (2)  Changed b functions to mem functions for solaris compatibility
        (3)  Fixed default values in spreadsheet help

 * Revision 1.17  1995/03/20  22:44:35  markstro
 * DG changes
 *
 * Revision 1.16  1995/02/10  23:58:25  markstro
 * Bug fixes for class
 *
 * Revision 1.15  1995/02/01  17:47:21  markstro
 * Addition of Rosenbrock optimization.  Start of sensitivity.  Many bug fixes.
 *
 * Revision 1.14  1994/11/25  18:13:40  markstro
 * unknown
 *
 * Revision 1.13  1994/11/22  17:19:25  markstro
 * (1) Cleaned up dimensions and parameters.
 * (2) Some changes due to use of malloc_dbg.
 *
 * Revision 1.12  1994/10/24  14:18:18  markstro
 * (1)  Integration of CADSWES's work on GIS.
 * (2)  Prototypes were added to the files referenced in "mms_proto.h".
 *
 * Revision 1.11  1994/10/13  17:53:35  markstro
 * (1) Added annotation to parameter values through the spreadsheet
 * (2) Included <string.h> in a few more files that needed it.
 *
 * Revision 1.10  1994/09/30  14:54:08  markstro
 * Initial work on function prototypes.
 *
 * Revision 1.9  1994/09/20  21:58:43  markstro
 * Got rid of some compiler warnings
 *
 * Revision 1.8  1994/09/13  16:23:13  markstro
 * Added "bounded" check to parameter db verification.
 *
 * Revision 1.7  1994/09/13  15:20:21  markstro
 * (1) Check to make sure that parameters being declared are consistent wit
 *     parameters already declared.
 * (2) Ran through cb and put in headers.
 *
 * Revision 1.6  1994/06/16  16:47:06  markstro
 * Worked over runcontrol.c
 *
 * Revision 1.5  1994/06/13  18:40:27  markstro
 * When there are declarations of the same parameter from different modules
 * there is no longer an error message.  The modules now just use the same
 * entry in the parameter DB.
 *
 * Revision 1.4  1994/02/01  21:17:12  markstro
 * Unknown
 *
 * Revision 1.3  1994/02/01  17:41:25  markstro
 * Made the declaration of parameters dynamic -- no more MAXPARAMS
 *
 * Revision 1.2  1994/01/31  20:16:09  markstro
 * Make sure that all source files have CVS log.
 *
-*/

/**1************************ INCLUDE FILES ****************************/
#define DECLPARAM_C
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "mms.h"

/**2************************* LOCAL MACROS ****************************/

/**3************************ LOCAL TYPEDEFS ***************************/

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/
static int CHECK_param_in_db (char *, char *, char *, int,
 	char *, char *, char *, char *, char *, char *);
static int VAR_type (char *);

/**5*********************** LOCAL VARIABLES ***************************/
static char *types[] = {"long (or integer)", "real (or float)", "double", "string"};

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/

/*--------------------------------------------------------------------*\
 | FUNCTION     : declparam_
 | COMMENT		: declparam_() is called from Fortran, sorts out args
 |                 and calls declparam()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declparam_u_ (char *mname, char *pname, char *pdimen, char *ptype,
	char *pvalstr, char *minstr, char *maxstr, char *dstr, char *hstr,
	char *ustr, char *var, long *update, ftnlen mnamelen,
	ftnlen pnamelen, ftnlen pdimenlen, ftnlen ptypelen,
	ftnlen pvallen, ftnlen minlen, ftnlen maxlen, ftnlen dlen,
	ftnlen hlen, ftnlen ulen, ftnlen varlen, ftnlen uplen) {

	char *module, *name, *dimen, *type, *value;
	char *minimum, *maximum, *descr, *help, *units;
	long retval;

/*
* copy args to new strings, and terminate correctly
*/

	module = (char *) umalloc(mnamelen + 1);
	strncpy(module, mname, mnamelen);
	module[mnamelen] = '\0';

	name = (char *) umalloc(pnamelen + 1);
	strncpy(name, pname, pnamelen);
	name[pnamelen] = '\0';

	dimen = (char *) umalloc(pdimenlen + 1);
	strncpy(dimen, pdimen, pdimenlen);
	dimen[pdimenlen] = '\0';

	type = (char *) umalloc(ptypelen + 1);
	strncpy(type, ptype, ptypelen);
	type[ptypelen] = '\0';

	value = (char *) umalloc(pvallen + 1);
	strncpy(value, pvalstr, pvallen);
	value[pvallen] = '\0';

	minimum = (char *) umalloc(minlen + 1);
	strncpy(minimum, minstr, minlen);
	minimum[minlen] = '\0';

	maximum = (char *) umalloc(maxlen + 1);
	strncpy(maximum, maxstr, maxlen);
	maximum[maxlen] = '\0';

	descr = (char *) umalloc(dlen + 1);
	strncpy(descr, dstr, dlen);
	descr[dlen] = '\0';

	help = (char *) umalloc(hlen + 1);
	strncpy(help, hstr, hlen);
	help[hlen] = '\0';

	units = (char *) umalloc(ulen + 1);
	strncpy(units, ustr, ulen);
	units[ulen] = '\0';

/*
* call C version of declparam_u()
*/

	retval = declparam_u(module, name, dimen, type, value,
	    minimum, maximum, descr, help, units, var, update);

	return(retval);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declparam_u
 | COMMENT		: declparam is called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declparam_u (char *module, char *name, char *dimen, char *type, char *value,
	char *minimum, char *maximum, char *descr, char *help, char *units, char *var,
	long *update) {

	PARAM *param;

	*update = 0;
/*
* get pointer to parameter with key
*/

	param = param_addr(name);

	if (param == NULL) {  // Parameter has not been declared, do so now. Set up array for pointers to local arrays of values.
		declparam (module, name, dimen, type, value, minimum, maximum, descr, help, units);
		param = param_addr(name);
		param->num_references = 0;
		param->size_references = 100;
		param->references = (void **)umalloc (param->size_references * sizeof(void *));
	}

/*
**	 realloc if too large
*/
	if (param->num_references >= param->size_references - 1) {
		param->size_references += 100;
		param->references = (void **) urealloc ((char *)(param->references),
			param->size_references * sizeof(void *));
	}

	param->references[param->num_references++] = var;

	//((float *)(var))[0] = 1234.5;
	//((float *)(var))[1] = 234.5;

	return 0;
}


/*--------------------------------------------------------------------*\
 | FUNCTION     : declparam_
 | COMMENT		: declparam_() is called from Fortran, sorts out args
 |                 and calls declparam()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declparam_ (char *mname, char *pname, char *pdimen, char *ptype,
	char *pvalstr, char *minstr, char *maxstr, char *dstr, char *hstr,
	char *ustr, ftnlen mnamelen, ftnlen pnamelen, ftnlen pdimenlen, ftnlen ptypelen,
	ftnlen pvallen, ftnlen minlen, ftnlen maxlen, ftnlen dlen, ftnlen hlen, ftnlen ulen) {

	char *module, *name, *dimen, *type, *value;
	char *minimum, *maximum, *descr, *help, *units;
	long retval;

/*
* copy args to new strings, and terminate correctly
*/

	module = (char *) umalloc(mnamelen + 1);
	strncpy(module, mname, mnamelen);
	module[mnamelen] = '\0';

	name = (char *) umalloc(pnamelen + 1);
	strncpy(name, pname, pnamelen);
	name[pnamelen] = '\0';

	dimen = (char *) umalloc(pdimenlen + 1);
	strncpy(dimen, pdimen, pdimenlen);
	dimen[pdimenlen] = '\0';

	type = (char *) umalloc(ptypelen + 1);
	strncpy(type, ptype, ptypelen);
	type[ptypelen] = '\0';

	value = (char *) umalloc(pvallen + 1);
	strncpy(value, pvalstr, pvallen);
	value[pvallen] = '\0';

	minimum = (char *) umalloc(minlen + 1);
	strncpy(minimum, minstr, minlen);
	minimum[minlen] = '\0';

	maximum = (char *) umalloc(maxlen + 1);
	strncpy(maximum, maxstr, maxlen);
	maximum[maxlen] = '\0';

	descr = (char *) umalloc(dlen + 1);
	strncpy(descr, dstr, dlen);
	descr[dlen] = '\0';

	help = (char *) umalloc(hlen + 1);
	strncpy(help, hstr, hlen);
	help[hlen] = '\0';

	units = (char *) umalloc(ulen + 1);
	strncpy(units, ustr, ulen);
	units[ulen] = '\0';

/*
* call C version of declparam()
*/

	retval = declparam(module, name, dimen, type, value,
	    minimum, maximum, descr, help, units);

/*
* free up strings 
*/

//      ufree(module);
//      ufree(name);
//      ufree(dimen);
//      ufree(type);
//      ufree(value);
//      ufree(minimum);
//      ufree(maximum);
//      ufree(descr);
//      ufree(help);
//      ufree(units);

	return(retval);

}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declparam
 | COMMENT		: declparam is called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declparam (char *module, char *name, char *dimen, char *type, char *value,
	char *minimum, char *maximum, char *descr, char *help, char *units) {

	int var_type;
	char *pkey;
	char *token;
	char *tmpdimen;
	long i, retval;

	DIMEN *dim;
	PARAM **params, *param;

/*
**	 realloc if too large
*/
	if (Mnparams >= max_params - 1) {
		max_params += 100;
		Mparambase = (PARAM **) urealloc ((char *)Mparambase,
			max_params * sizeof(PARAM *));
	}

/*
* compute the key
*/
/*
  pkey = (char *) umalloc(strlen(module) + strlen(name) + 2);
  (void)strcpy(pkey, module);
  strcat(strcat(pkey, "."), name);
*/
	pkey = strdup (name);

	if (!(var_type = VAR_type (type)))
		return (0);

	// DANGER - markstro - this overrides the module name that is passed in
	// from the module and replaces it with the name of the last module that
	// called declmodule
	module = current_module->name;
	ADD_to_list (current_module->params, pkey);

	if (CHECK_param_in_db (pkey, module, dimen, var_type, value,
									minimum, maximum, descr, help, units)) {
		return (0);
	}

	if (Mdebuglevel >= M_FULLDEBUG)
		(void)fprintf (stderr, "Declaring param '%s'\n", pkey);
/* 
* get params from Mparambase, the global pointer
*/
	params = Mparambase;
	Mnparams += 1;

/*
* allocate space for a structure, and store pointer in params
*/
	params[Mnparams-1] = param = (PARAM *) umalloc (sizeof(PARAM));
	memset ((char *)param, 0, sizeof(PARAM));

	param->key = pkey;
	param->module = strdup (module);
	param->name = strdup (name);
	param->min_string = strdup (minimum);
	param->max_string = strdup (maximum);
	param->def_string = strdup (value);
	param->value_string = strdup (value);
	param->descr = strdup (descr);
	param->help = strdup (help);
	param->units = strdup (units);
	param->def = strdup (value);
	param->column_width = 4;
	param->type = var_type;
	param->read_in = 0;
	param->preprocess = FALSE;

/*
* determine dimensions
*/
	tmpdimen = strdup (dimen);
	token = strtok (tmpdimen, ",");

	while (token != (char *) NULL) {
		param->ndimen++;
		token = strtok((char *) NULL, ",");
	}

	if (param->ndimen > MAX_NDIMEN) {
		(void)fprintf(stderr, "ERROR - declparam\n");
		(void)fprintf(stderr, "Attempt to use %ld dimensions - this is too many.\n",
		    param->ndimen);
		(void)fprintf(stderr, "Max number of dimensions allowed : %d.\n", MAX_NDIMEN);
		(void)fprintf(stderr, "Key is '%s'.\n", pkey);
		return(1);
	}

	param->dimen = (DIMEN **)umalloc (param->ndimen * sizeof (DIMEN *));

	(void)strcpy (tmpdimen, dimen);
	token = strtok (tmpdimen, ",");

	i = 0;
	while (token != (char *) NULL) {
		param->dimen[i++] = dim_addr (token);
		token = strtok ((char *) NULL, ",");
	}
//      ufree (tmpdimen);

/*
* check to see if the parameter values are to be bounded by a dimension.
* If so, set the bound and bound_dimen fields, and set the min and max strings
* as applicable. If bounded, the 'minimum' string is set to "bounded",
* and the 'maximum' string contains the name of the bounding dimension.
*/
	if (!strcmp (minimum, "bounded")) {
		param->bound_status = M_BOUNDED;

		if (param->type != M_LONG) {
			(void)fprintf (stderr, "ERROR - declparam\n");
			(void)fprintf (stderr, "Parameter '%s'\n", pkey);
			(void)fprintf (stderr,
			    "Attempt to bound with parameter type '%s'\n",
			    Mtypes[param->type]);
			(void)fprintf(stderr, "Only 'long' parameters may be bounded.\n");
			return(1);
		}

		if (!(param->bound_dimen = dim_addr (maximum))) {
			(void)fprintf(stderr, "ERROR - declparam\n");
			(void)fprintf(stderr, "Parameter '%s'\n", pkey);
			(void)fprintf(stderr, "Attempt to bound with dimension name '%s' %s\n",
			    maximum, "which has not been declared.");
			return(1);
		}
	} else {
		param->bound_status = M_UNBOUNDED;
		param->bound_dimen = NULL;
	}

/*
* get the size of the parameter
*/
	param->size = 1;
	for (i = 0; i < param->ndimen; i++) {
		dim = param->dimen[i];
		if (dim) {
			param->size *= dim->value;
		} else {
			(void)fprintf (stderr, "ERROR - declparam\n");
			(void)fprintf (stderr, "Parameter '%s'\n", pkey);
			(void)fprintf (stderr, "Dimension '%s' not declared.\n", dim->name);
			return (1);
		}
	}

/*
* if size is zero, set size to 1 so that there is at least one
* entry in the data base. This is necesary so that the default, 
* maximum and minimum values will be retained for use when
* the dimension is set to a non-zero value
*/
	if (!param->size)
		param->size = 1;

/*
**	Load the values indicated by the default, minimum, and maximum
**	strings into the arrays.
*/
	retval = load_param (param);
	if (retval) return (retval);

/*
**	Set up the pointers to the description strings.
*/
	param->value_desc = (char **)umalloc (param->size * sizeof (char *));
	for (i = 0; i < param->size; i++)
		param->value_desc[i] = NULL;

	sort_params();
	return(0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declparam_p_
 | COMMENT		: declparam_p() is called from Fortran, sorts out args
 |                 and calls declparam()
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declparam_p_ (char *mname, char *pname, char *pdimen, char *ptype,
	char *pvalstr, char *minstr, char *maxstr, char *dstr, char *hstr,
	char *ustr, char *val, ftnlen mnamelen, ftnlen pnamelen,
	ftnlen pdimenlen, ftnlen ptypelen, ftnlen pvallen, ftnlen minlen,
	ftnlen maxlen, ftnlen dlen, ftnlen hlen, ftnlen ulen, ftnlen vallen) {

	char *module, *name, *dimen, *type, *value;
	char *minimum, *maximum, *descr, *help, *units;
	long retval;

/*
* copy args to new strings, and terminate correctly
*/

	module = (char *) umalloc(mnamelen + 1);
	strncpy(module, mname, mnamelen);
	module[mnamelen] = '\0';

	name = (char *) umalloc(pnamelen + 1);
	strncpy(name, pname, pnamelen);
	name[pnamelen] = '\0';

	dimen = (char *) umalloc(pdimenlen + 1);
	strncpy(dimen, pdimen, pdimenlen);
	dimen[pdimenlen] = '\0';

	type = (char *) umalloc(ptypelen + 1);
	strncpy(type, ptype, ptypelen);
	type[ptypelen] = '\0';

	value = (char *) umalloc(pvallen + 1);
	strncpy(value, pvalstr, pvallen);
	value[pvallen] = '\0';

	minimum = (char *) umalloc(minlen + 1);
	strncpy(minimum, minstr, minlen);
	minimum[minlen] = '\0';

	maximum = (char *) umalloc(maxlen + 1);
	strncpy(maximum, maxstr, maxlen);
	maximum[maxlen] = '\0';

	descr = (char *) umalloc(dlen + 1);
	strncpy(descr, dstr, dlen);
	descr[dlen] = '\0';

	help = (char *) umalloc(hlen + 1);
	strncpy(help, hstr, hlen);
	help[hlen] = '\0';

	units = (char *) umalloc(ulen + 1);
	strncpy(units, ustr, ulen);
	units[ulen] = '\0';

/*
* call C version of declparam_p()
*/

	retval = declparam_p(module, name, dimen, type, value,
	    minimum, maximum, descr, help, units, val);

	return(0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : declparam_p
 | COMMENT		: declparam is called from C
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
long declparam_p (char *module, char *name, char *dimen, char *type, char *value,
	char *minimum, char *maximum, char *descr, char *help, char *units, char *var) {
	PARAM *param;

	// If the -preprocess command line arguement is not set, don't allow declaration of any "preprocess parameters."
	if (!preprocess_on) {
		return 0;
	}

/*
* get pointer to parameter with key
*/

	param = param_addr(name);

	if (param == NULL) {  // Parameter has not been declared, do so now. Set up array for pointers to local arrays of values.
		declparam (module, name, dimen, type, value, minimum, maximum, descr, help, units);
		param = param_addr(name);
		param->num_references = 0;
		param->size_references = 100;
		param->references = (void **)umalloc (param->size_references * sizeof(void *));
		param->preprocess = TRUE;
	}

/*
**	 realloc if too large
**  LOOK AT THIS!
*/
	if (param->num_references >= param->size_references - 1) {
		param->size_references += 100;
		param->references = (void **) urealloc ((char *)(param->references),
			param->size_references * sizeof(void *));
	}

	param->references[param->num_references++] = var;

	//((float *)(var))[0] = 1234.5;
	//((float *)(var))[1] = 234.5;

	return 0;
}

/**7****************** LOCAL FUNCTION DEFINITIONS *********************/
/*--------------------------------------------------------------------*\
 | FUNCTION     : CHECK_param_in_db
 | COMMENT      : Check if this parameter is already in the parameter DB.
 | PARAMETERS   : char *pkey -  parameter key
 | RETURN VALUE : 0 = not found; 1 = found
 | RESTRICTIONS : None
\*--------------------------------------------------------------------*/
static int CHECK_param_in_db (char *pkey, char *module, char *dimen, int var_type,
 	char *value, char *minimum, char *maximum, char *descr, char *help,
 	char *units) {

	PARAM *check_param;
	int		inconsistent, i;
	char	buf[1024], buf1[256];
	char dim_names[256];

	check_param = param_addr (pkey);
	if (check_param) {

		inconsistent = FALSE;

		(void)sprintf (buf, "The parameter %s has been declared inconsistently in the modules %s and %s.", pkey, module, check_param->module);

		/*
		 * Get all dimensions of previously declared parameters in the
		 * original format.
		 */
		strcpy(dim_names, check_param->dimen[0]->name);
		for (i = 1; i < check_param->ndimen; i++){
		  sprintf(dim_names,"%s,%s",dim_names,check_param->dimen[i]->name);
		}
		if (strcmp (dimen, dim_names)) {
			inconsistent = TRUE;
			(void)sprintf (buf1, " The dimensions have been declared as %s and %s.", dimen, dim_names);
			strcat (buf, buf1);
		}

		if (var_type != check_param->type) {
			inconsistent = TRUE;
			(void)sprintf (buf1, " The types have been declared as %s and %s.", types[var_type], types[check_param->type]);
			strcat (buf, buf1);
		}

		if (strcmp (value, check_param->value_string)) {
			inconsistent = TRUE;
			(void)sprintf (buf1, " The default values have been declared as %s and %s.", value, check_param->value_string);
			strcat (buf, buf1);
		}

		if (strcmp (minimum, check_param->min_string)) {
			inconsistent = TRUE;
			(void)sprintf (buf1, " The minimum value has been declared as %s and %s.", minimum, check_param->min_string);
			strcat (buf, buf1);
		}

		if (strcmp (maximum, check_param->max_string)) {
			inconsistent = TRUE;
			(void)sprintf (buf1, " The maximum value has been declared as %s and %s.", maximum, check_param->max_string);
			strcat (buf, buf1);
		}

		if (strcmp (descr, check_param->descr)) {
			inconsistent = TRUE;
			(void)sprintf (buf1, " The parameter description has been set as \"%s\"and \"%s.\"", descr, check_param->descr);
			strcat (buf, buf1);
		}

		if (strcmp (units, check_param->units)) {
			inconsistent = TRUE;
			(void)sprintf (buf1, " The units have been set as %s and %s.", help, check_param->help);
			strcat (buf, buf1);
		}

		if (inconsistent)
		    fprintf(stderr, buf);

		return (1);
	}

	return (0);
}

/*--------------------------------------------------------------------*\
 | FUNCTION     : VAR_type
 | COMMENT      :
 | PARAMETERS   :
 | RETURN VALUE : 
 | RESTRICTIONS :
\*--------------------------------------------------------------------*/
static int VAR_type (char *type) {
	if (!strcmp (type, "integer") || !strcmp (type, "long")) {
		return (M_LONG);
	}

	if (!strcmp (type, "real") || !strcmp (type, "float")) {
	   return (M_FLOAT);
    }

	if (!strcmp (type, "double precision") || !strcmp (type, "double")) {
	   return (M_DOUBLE);
    }

	if (!strcmp (type, "string")) {
	   return (M_STRING);
    }
	(void)fprintf(stderr, "ERROR - declparam - type '%s' is illegal.\n", type);
		return (0);
}

/**8************************** TEST DRIVER ****************************/


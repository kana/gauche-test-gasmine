/*
 * gauche_gasmine.c
 */

#include "gauche_gasmine.h"

/*
 * The following function is a dummy one; replace it for
 * your C function definitions.
 */

ScmObj test_gauche_gasmine(void)
{
    return SCM_MAKE_STR("gauche_gasmine is working");
}

/*
 * Module initialization function.
 */
extern void Scm_Init_gauche_gasminelib(ScmModule*);

void Scm_Init_gauche_gasmine(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(gauche_gasmine);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("gauche.test.gasmine", TRUE));

    /* Register stub-generated procedures */
    Scm_Init_gauche_gasminelib(mod);
}

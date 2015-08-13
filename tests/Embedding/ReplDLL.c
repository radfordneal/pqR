/* Adapted from the example in R-exts.texi */

#include <Rinternals.h>
#include <Rembedded.h>

int main(int ac, char **av)
{
    Rprintf("Stage 1\n");
    Rf_initEmbeddedR(argc, argv);

    Rprintf("Stage 2\n");
    R_ReplDLLinit();

    Rprintf("Stage 3\n");
    while (R_ReplDLLdo1() > 0) {
        if (TYPEOF(R_CurrentExpr) == LGLSXP) 
            Rprintf("Logical!\n");
    }

    Rprintf("Stage 4\n");
    Rf_endEmbeddedR(0);

    Rprintf("Stage 5\n");
    return 0;
}

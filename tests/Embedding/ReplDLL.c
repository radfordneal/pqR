/* An example in the Writing R Extensions manual */

#include <Rinternals.h>
#include <Rembedded.h>

int main(int argc, char **argv)
{
    Rf_initEmbeddedR(argc, argv);
    R_ReplDLLinit();

    while (R_ReplDLLdo1() > 0) {
        if (TYPEOF(SYMVALUE(R_LastvalueSymbol)) == LGLSXP) 
            Rprintf("Logical!\n");
    }

    Rf_endEmbeddedR(0);
    return 0;
}

/* An example in the Writing R Extensions manual */

#include <Rinternals.h>
#include <Rembedded.h>

int main(int argc, char **argv)
{
    Rf_initEmbeddedR(argc, argv);
    R_ReplDLLinit();

    for (;;) {
        int status;
        status = R_ReplDLLdo1();
        if (status < 0)  /* EOF */
            break;
        else if (status == 2)  /* error trapped at top level */
            Rprintf("Oops!\n");     /* example of extra error action */
        else if (TYPEOF(SYMVALUE(R_LastvalueSymbol)) == LGLSXP) 
            Rprintf("Logical!\n");  /* another example of an extra action */
    }

    Rf_endEmbeddedR(0);
    return 0;
}

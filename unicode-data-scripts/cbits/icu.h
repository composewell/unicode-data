#include <unicode/utypes.h>
#include <unicode/uscript.h>

UScriptCode __hs_uscript_getScript(UChar32 codepoint);

int32_t __hs_uscript_getScriptExtensions
    ( UChar32 codepoint
    , UScriptCode * scripts
    , int32_t capacity );

const char * __hs_uscript_getShortName(UScriptCode scriptCode);

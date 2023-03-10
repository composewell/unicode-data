#include <unicode/utypes.h>
#include "icu.h"

UScriptCode __hs_uscript_getScript(UChar32 codepoint) {
    UErrorCode err = U_ZERO_ERROR;
    return uscript_getScript(codepoint, &err);
}

int32_t __hs_uscript_getScriptExtensions
    ( UChar32 codepoint
    , UScriptCode * scripts
    , int32_t capacity ) {
    UErrorCode err = U_ZERO_ERROR;
    return uscript_getScriptExtensions(codepoint, scripts, capacity, &err);
}

const char * __hs_uscript_getShortName(UScriptCode scriptCode) {
    return uscript_getShortName(scriptCode);
}

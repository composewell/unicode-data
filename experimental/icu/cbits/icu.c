#include <unicode/utypes.h>
#include <unicode/uchar.h>
#include <unicode/uscript.h>
#include "icu.h"

void __hs_u_getUnicodeVersion(UVersionInfo versionArray) {
    u_getUnicodeVersion(versionArray);
}

/*******************************************************************************
 * Properties
 ******************************************************************************/

bool __hs_u_hasBinaryProperty(UChar32 c, UProperty which) {
    return u_hasBinaryProperty(c, which);
}

/*******************************************************************************
 * Names
 ******************************************************************************/

int32_t __hs_u_charName( UChar32 codepoint
                       , UCharNameChoice nameChoice
                       , char * buffer
                       , int32_t bufferLength) {
    UErrorCode err = U_ZERO_ERROR;
    return u_charName(codepoint, nameChoice, buffer, bufferLength, &err);
}

void __hs_u_charAge( UChar32 c, UVersionInfo versionArray ) {
    u_charAge(c, versionArray);
}

int8_t __hs_u_charType(UChar32 c) {
    return u_charType(c);
}

/*******************************************************************************
 * Scripts
 ******************************************************************************/

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

int __hs_getMaxScript(void) {
    return u_getIntPropertyMaxValue(UCHAR_SCRIPT);
}

const char * __hs_uscript_getShortName(UScriptCode scriptCode) {
    return uscript_getShortName(scriptCode);
}

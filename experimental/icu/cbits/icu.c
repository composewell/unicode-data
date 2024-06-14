#include <unicode/utypes.h>
#include <unicode/uchar.h>
#include "icu.h"

void __hs_u_getUnicodeVersion(UVersionInfo versionArray) {
    u_getUnicodeVersion(versionArray);
}

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

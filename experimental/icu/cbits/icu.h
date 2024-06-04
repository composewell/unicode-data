#ifndef _ICU_NAMES_H
#define _ICU_NAMES_H

#include <unicode/utypes.h>
#include <unicode/uchar.h>

void __hs_u_getUnicodeVersion(UVersionInfo versionArray);

int32_t __hs_u_charName( UChar32 codepoint
                       , UCharNameChoice nameChoice
                       , char * buffer
                       , int32_t bufferLength );

// typedef uint8_t UVersionInfo[U_MAX_VERSION_LENGTH];
void __hs_u_charAge( UChar32 c, UVersionInfo versionArray );

static const int __hs_U_MAX_VERSION_LENGTH = U_MAX_VERSION_LENGTH;
static const int __hs_U_UNICODE_CHAR_NAME  = U_UNICODE_CHAR_NAME;
static const int __hs_U_CHAR_NAME_ALIAS    = U_CHAR_NAME_ALIAS;

#endif

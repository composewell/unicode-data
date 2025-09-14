#ifndef _ICU_NAMES_H
#define _ICU_NAMES_H

#include <unicode/utypes.h>
#include <unicode/uchar.h>
#include <unicode/uscript.h>

void __hs_u_getUnicodeVersion(UVersionInfo versionArray);

/*******************************************************************************
 * Properties
 ******************************************************************************/

bool __hs_u_hasBinaryProperty(UChar32 c, UProperty which);

/*******************************************************************************
 * Names
 ******************************************************************************/

int32_t __hs_u_charName( UChar32 codepoint
                       , UCharNameChoice nameChoice
                       , char * buffer
                       , int32_t bufferLength );
static const int __hs_U_UNICODE_CHAR_NAME  = U_UNICODE_CHAR_NAME;
static const int __hs_U_CHAR_NAME_ALIAS    = U_CHAR_NAME_ALIAS;

// typedef uint8_t UVersionInfo[U_MAX_VERSION_LENGTH];
void __hs_u_charAge( UChar32 c, UVersionInfo versionArray );
static const int __hs_U_MAX_VERSION_LENGTH = U_MAX_VERSION_LENGTH;

int8_t __hs_u_charType(UChar32 c);

/*******************************************************************************
 * Scripts
 ******************************************************************************/

UScriptCode __hs_uscript_getScript(UChar32 codepoint);

int32_t __hs_uscript_getScriptExtensions
    ( UChar32 codepoint
    , UScriptCode * scripts
    , int32_t capacity );

int __hs_getMaxScript(void);

const char * __hs_uscript_getShortName(UScriptCode scriptCode);

#endif

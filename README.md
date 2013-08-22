web-escape
==========

This implements the [OWASP recommendations on preventing XSS](https://www.owasp.org/index.php/XSS_%28Cross_Site_Scripting%29_Prevention_Cheat_Sheet), escaping all control characters. While there are many libraries for escaping HTML, most are buried within libraries with heavier dependencies; this aims to be light and relatively portable.

This library provides escaping functions for the following contexts:
 - HTML (within an element): `escapeHTML`
 - HTML (within an attribute): `escapeAttribute`
 - JavaScript (inside data value): `escapeJS`
 - CSS (within a property value): `escapeCSS`
 - URLs: escapeURL

Limitations
-----------

This library does not blanketly escape unicode; if text contains unicode, the caller remains responsible for ensuring its proper delivery. The (partial) esception is `escapeURL`; this will percent-encode characters above 255 but truncate the representation (since there is no accepted standard for representation of unicode in URLs). If using unicode in URL parameters, encode it (such as in base64) first.

This library also does not protect against semantic attacks, such as giving a script:// URL as an img src attribute.

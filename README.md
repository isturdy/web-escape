web-escape
==========

This implements the [OWASP recommendations on preventing XSS](https://www.owasp.org/index.php/XSS_%28Cross_Site_Scripting%29_Prevention_Cheat_Sheet), escaping all control characters. While there are many libraries for escaping HTML, most are buried within libraries with heavier dependencies; this aims to be light and portable.

This library provides escaping functions for the following contexts:
 - HTML (within an element): escapeHTML
 - HTML (within an attribute): escapeAttribute
 - JavaScript (inside data value): escapeJS
 - JSON (within an HTML element): escapeJSON
 - CSS (within a property value): escapeCSS
 - URLs: escapeURL

Note that this library does not attempt to sanitize HTML (instead escaping all control characters), nor does it protect against semantic vulnerabilities (such as a user providing link to a script in CSS).

# Parsebib #


(c) 2014-2021 Joost Kremers

`Parsebib` is an Elisp library for reading bibliographic database files. It supports both BibTeX / `biblatex` (`.bib`)  files and CSL-JSON (`.json`) files.

The library provides functions that parse the current buffer. They are intended to be used inside a `with-temp-buffer` combined with `insert-file-contents`, but they can also be used in a buffer visiting a bibliography file, of course.

The bibliographic data is returned as a hash table. To parse multiple files, you can either insert them all into one temp buffer, or pass the hash table obtained by parsing the first buffer as argument when passing the next buffer.



## BibTeX / `biblatex` ##

Support for `.bib` files comes in two different APIs, a higher-level one that reads all items in one go, and a lower-level one that reads one item at a time. `Parsebib` supports `@Preamble`, `@String` and `@Comment` items, and obviously actual bibliographic entries.


### Resolving `@string` abbreviations and cross-references ###

Parsebib can resolve `@string` abbrevs and cross-references while reading the contents of a `.bib` file. When `@string` abbrevs are resolved, abbreviations in field values (or `@string` definitions) are replaced with their expansion. In addition, the braces or double quotes around field values are removed, and multiple spaces and newlines in sequence are reduced to a single space. In essence, the field values are modified in such a way that they are suitable for display, but they no longer reliably represent the contents of the `.bib` file. When `@string` abbrevs are not resolved, no modifications are applied to the field values, so that the parsing results reflect the contents of the `.bib` file accurately.

Cross-references can also be resolved. This means that if an entry that has a `crossref` field, fields in the cross-referenced entry that are not already part of the cross-referencing entry are added to it. Both BibTeX's (rather simplistic) inheritance rule and BibLaTeX's more sophisticated inheritance schema are supported. It is also possible to specify a custom inheritance schema. Note that resolving cross-references can be done independently from resolving `@string` abbrevs, but the former generally won't make sense without the latter.

Resolving `@string` abbrevs can be done with both the higher-level and the lower-level API. Resolving cross-references can only be done with the higher-level API. This is mainly because cross-referenced entries appear *after* cross-referencing entries in the `.bib` file, so that when an entry with a `crossref` field is read, its cross-referenced entry is not known yet, while `@string` definitions appear in the `.bib` file before they are used. It is possible, however, to resolve cross-references after all entries have been read.


### Higher-level API ###

The higher-level API consists of functions that read and return all items of a specific type in the current buffer. They do not move point.


#### `parsebib-collect-entries (&optional hash strings inheritance)` ####

Collect all entries in the current buffer and return them as a hash table, where the keys correspond to the BibTeX keys and the values are alists consisting of `(<field> . <value>)` pairs of the relevant entry. In this alist, the BibTeX key and the entry type are stored under `=key=` and `=type=`, respectively. Note that both `<field>` and `<value>` are strings. 

The argument `hash` can be used to pass a (possibly non-empty) hash table in which the entries are stored. This can be used to combine multiple `.bib` files into a single hash table, or to update an existing hash table by rereading its `.bib` file.

If the argument `strings` is present, `@string` abbreviations are expanded. `strings` should be a hash table of `@string` definitions as returned by `parsebib-collect-strings`.

If the argument `inheritance` is present, cross-references among entries are resolved. It can be `t`, in which case the file-local or global value of `bibtex-dialect` is used to determine which inheritance schema is used. It can also be one of the symbols `BibTeX` or `biblatex`, or it can be a custom inheritance schema.


#### `parsebib-collect-strings (&optional hash expand-strings)` ####

Collect all `@string` definitions in the current buffer and return them as a hash table. The argument `hash` can be used to provide a hash table to store the definitions in. If it is `nil`, a new hash table is created.

The argument `expand-strings` is a boolean value. If non-nil, any abbreviations found in the string definitions are expanded against the `@string` definitions appearing earlier in the `.bib` file and against `@string` definitions in `hash`, if provided.


#### `parsebib-collect-preambles` ####

Collect all `@preamble` definitions in the current buffer and return them as a list.


#### `parsebib-collect-comments` ####

Collect all `@comments` in the current buffer and return them as a list.


#### `parsebib-find-bibtex-dialect` ####

Find and return the BibTeX dialect for the current buffer. The BibTeX dialect is either `BibTeX` or `biblatex` and can be defined in a local-variable block at the end of the file.


#### `parsebib-parse-bib-buffer (&optional entries strings expand-strings inheritance)` ####

Collect all BibTeX data in the current buffer. Return a five-element list:

    (<entries> <strings> <preambles> <comments> <BibTeX dialect>)

The `<entries>` and `<strings>` are hash tables, `<preambles>` and `<comments>` are lists, `<BibTeX dialect>` is a symbol (either `BibTeX` or `biblatex`) or `nil`.

If the arguments `entries` and `strings` are present, they should be hash tables with `equal` as the `:test` function. They are then used to store the entries and strings, respectively.

The argument `expand-strings` functions as the same-name argument in `parsebib-collect-strings`, and `inheritance` functions as the same-name argument in `parsebib-collect-entries`.

Note that `parsebib-parse-bib-buffer` only makes one pass through the buffer. It is therefore a bit faster than calling all the `parsebib-collect-*` functions above in a row, since that would require making four passes through the buffer.


#### `parsebib-expand-xrefs (entries inheritance)` ####

Expand cross-references in `entries` according to inheritance schema `inheritance`. `entries` should be a hash table as returned by `parsebib-collect-entries`. Each entry with a `crossref` field is expanded as described above. The results are stored in the hash table `entries` again, the return value of this function is always `nil`.


### Lower-level API ###

The lower-level API consists of functions that do the actual reading of a BibTeX item. Unlike the higher-level API, the functions here are dependent on the position of `point`. They are meant to be used in a `while` loop in which `parsebib-find-next-item` is used to move `point` to the next item and then use one of the `parsebib-read-*` functions to read the contents of the item.

All functions here take an optional position argument, which is the position in the buffer from which they should start reading. The default value is `(point)`.


#### `parsebib-find-next-item (&optional pos)` ####

Find the first BibTeX item following `pos`, where an item is either a BibTeX entry, or a `@Preamble`, `@String`, or `@Comment`. This function returns the item's type as a string, i.e., either `"preamble"`, `"string"`, or `"comment"`, or the entry type. Note that the `@` is *not* part of the returned string. This function moves point into the correct position to start reading the actual contents of the item, which is done by one of the following functions.


#### `parsebib-read-string (&optional pos strings)` ####
#### `parsebib-read-entry (type &optional pos strings)` ####
#### `parsebib-read-preamble (&optional pos)` ####
#### `parsebib-read-comment (&optional pos)` ####

These functions do what their names suggest: read one single item of the type specified. Each takes the `pos` argument just mentioned. In addition, `parsebib-read-string` and `parsebib-read-entry` take an extra argument, a hash table of `@string` definitions. When provided, abbreviations in the `@string` definitions or in field values are expanded. Note that `parsebib-read-entry` takes the entry type (as returned by `parsebib-find-next-entry`) as argument.

The reading functions return the contents of the item they read: `parsebib-read-preamble` and `parsebib-read-comment` return the text as a string. `parsebib-read-string` returns a cons cell of the form `(<abbrev> . <string>)`, and `parsebib-read-entry` returns the entry as an alist of `(<field> . <value>)` pairs. One of these pairs contains the entry type `=type=`, and one contains the entry key. These have the keys `"=key="` and `"=type="`, respectively.

Note that all `parsebib-read*` functions move point to the end of the entry.

The reading functions return `nil` if they do not find the element they should be reading at the line point is on. Point is nonetheless moved, however. Similarly, `parsebib-find-next-item` returns `nil` if it finds no next entry, leaving point at the end of the buffer. Additionally, it will signal an error of type `parsebib-entry-type-error` if it finds something that it deems to be an invalid item name. What is considered to be a valid name is determined by the regexp `parsebib-bibtex-identifier`, which is set to `"[^^\"@\\&$#%',={}() \t\n\f]*"`, meaning that any string not containing whitespace or any of the characters `^"@\&$#%',={}()` is considered a valid identifier.


## CSL-JSON ##

The support for CLS-JSON files comprises just one function: `parsebib-parse-json-buffer`. The actual parsing of the JSON data is performed by Emacs itself, either by the native JSON parsing routines (starting with Emacs 27.1, if available), or the built-in Elisp library `json.el`. `Parsebib` makes sure that the data is returned in a format that is similar to what is returned for `.bib` files.

Note, however, that the data format is not identical. For one, the entry types and field names are different. Especially relevant is the fact that in BibTeX data, the entry type and entry key are stored in the alist under `=type=` and `=key=`, while the same information is available in CSL-JSON data under `type` and `id`, respectively. Furthermore, it is important to keep in mind that in BibTeX data, the field names in an alist representing an entry are strings, while in CSL-JSON data, they are symbols.

As a last point, the field values of an entry in BibTeX are always returned as strings, whereas the values in CSL-JSON data may be strings, numbers, or alists. The caller can request that all values be converted to strings, however.


#### `parsebib-parse-json-buffer (&optional entries stringify year-only)` ####

Collect all CSL-JSON data in the current buffer and return the result. The return value is a hash table, where the keys correspond to the identifiers of the entries and the values are alists consisting of `(<field> . <value>)` pairs of the relevant entry. In this alist, the identifier is stored under the key `id` and the entry type is stored under `type`. `<field>` is a symbol, while `<value>` can be a string, a vector (array) or another alist.

The argument `entries` can be used to pass a (possibly non-empty) hash table in which the entries are stored. This can be used to combine multiple `.json` files into a single hash table, or to update an existing hash table by rereading its `.json` file.

Some fields in CLS-JSON are not strings. These are primarily name and date fields, which in CSL-JSON are represented as JSON objects. The argument `stringify` determines how they are returned. When `stringify` is set to `nil`, they are returned as alists; with `stringify` set to `t`, they are converted to strings.

The argument `stringify` is therefore similar to `expand-strings` and `inheritance` in `parsebib-parse-bib-buffer`, in that they return the bibliographic data in a form that is suitable for display but which does not represent the contents of the underlying `.json` file accurately.

The argument `year-only` controls the way dates are converted to strings. If it non-`nil`, only the year part is returned. See below for details.

The way values are converted to strings can be customised to some extent by the use of certain special variables, discussed below.

#### `parsebib-stringify-json (entry &optional year-only)`  ####

Convert the CSL-JSON data in `entry` to string form. `entry` is an alist as stored in the hash table returned by `parsebib-parse-json-buffer`. Return value is the modified `entry`.

Note that this function modifies `entry` *in place*. If you need to keep the original entry, call this function on a copy.

If the argument `year-only` is non-`nil`, date fields are shortened to just the year.

For details on the conversion, see below.


#### `parsebib-stringify-json-field (field &optional short)`  ####

Convert the value of `field` to string form and return the result. `field` is a cons cell of the form `(<field> . <value>)`, i.e., an element in the alist of an entry in the hash table returned by `parsebib-parse-json-buffer`.

The return value is a string representation of `<value>`. Note that unlike `parsebib-stringify-json`, this function does not modify its argument.

The following conversions are applied:

- Strings are returned unchanged.
- Numbers are converted to strings using `(format "%s" number)`.
- Name fields (as defined by `parsebib--json-name-fields`) are converted using `parsebib-json-name-field-template` and `parsebib-json-name-field-separator`; see below for details.
- Date fields (as defined by `parsebib--json-date-fields`) are converted to a format `2021-4-22`. If only a year is present, the month and day parts are omitted. The `season` and `circa` fields are accounted for, and so are `literal` and `raw`.
- Fields with an array as value (currently, in v1.0 of CSL-JSON, this only applies to the `categories` field), are converted to a string using `parsebib-json-field-separator`; see below for details.
- Anything that doesn't match any of the categories above is converted to a string using `(format "%s" value)`, after which any newlines are removed and replaced with a space. This is a catch-all that shouldn't be necessary in valid CSL-JSON files.

The optional argument `short` only applies to date fields. If `short` is non-nil, a date field contains just the year; month and day parts are ignored. If no year part can be found, `short` returns the string `XXXX`. Note that with `short`, other parts of the date field are ignored.

#### `parsebib-json-name-field-template` ####

This variable holds the template used for converting name fields. Its default value is:

```
"{non-dropping-particle }{family, }{given}{ dropping-particle}{, suffix}{literal}"
```

The elements of a name field appear in the string in the order provided in the template. Elements that are not part of a particular name field are ignored, including any punctuation and white space contained inside the braces. So for example, if a name field does not have a `suffix`, the comma-space appearing inside the `{, suffix}` part of the template is not included in the final string.

It is possible to modify the form of the resulting string by `let`-binding `parsebib-json-name-field-template` before calling `parsebib-parse-json-buffer`, `parsebib-stringify-json` or `parsebib-stringify-json-field`.

#### `parsebib-json-name-field-separator` ####

If more than one name appears in a name field, they are separated by the value of this variable in the string. The default value is `" and "` (note the spaces). It is possible to `let`-bind this variable.

#### `parsebib-json-field-separator` ####

Field values that are arrays are converted to a string using the value of this variable as a separator. Currently (CSL-JSON v1.0), this only applies to the `caterogies` field, which is an array of strings. The default value of this variable is `", "` (note the space). It can be `let`-bound like the variables above.

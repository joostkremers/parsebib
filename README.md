# Parsebib #


(c) 2014-2022 Joost Kremers

`Parsebib` is an Elisp library for reading bibliographic database files. It supports both BibTeX / `biblatex` (`.bib`)  files and CSL-JSON (`.json`) files.

The library provides functions that parse the current buffer. They are intended to be used inside a `with-temp-buffer` combined with `insert-file-contents`, but they can also be used in a buffer visiting a bibliography file, of course. Alternatively, the library provides a function `parsebib-parse` that takes a list of files and collects all the bibliographic data in them in. 

The data in the bibliography file can be returned in two ways. The first option is for `parsebib` to return the contents of the file accurately. This means that the field values as returned by `parsebib` are literally the field values in the file. This is useful if you need the contents of the `.bib` file literally, e.g., because your application provides the user with the option of modifying the data.

If you just want to display the bibliographic data, however, e.g., to allow a user to select a reference, you may want to represent the data in a way that is closer to what it would look like in a bibliography. To make this possible, `parsebib` can return the field values in such a way that they are suitable for display. For BibTeX / `biblatex` files, this means that `@String` abbreviations are expanded, cross-references are resolved and TeX markup is prettified or removed. For CSL-JSON files, it means that field values that are not strings (notably name and date fields) are converted to strings in a sensible way.


## BibTeX / `biblatex` vs. CSL-JSON ##

Although both are bibliography file formats, there are obviously differences between BibTeX / `biblatex` on the one hand and CSL-JSON on the other. The entry types and field names are different, and CSL-JSON does not have something similar to `@String` abbreviations or cross-references.

Especially relevant for the purpose of this library is that there are differences in the format of the data returned for the two types of files. For both formats, the bibliographic data is returned as a hash table. In this hash table, each entry is stored under its entry key (which is the `id` field in CSL-JSON) as an alist of `(<field> . <value>)` pairs. In BibTeX data, `<field>` is a string and field names are case-insensitive, so you may have `"Author"` or `"author"`, and both may occur in a single `.bib` file. Furthermore, `<value>` is always a string.

In CSL-JSON data, the format of the alist is slightly different: `<field>` is not a string but a symbol and the symbol name is case-sensitive. The CSL-JSON standard describes which field names are lower case (most are) and which are upper case (`ISBN`, `DOI`, etc.) Furthermore, the `<value>` part of the alist items may be a string, a number or a vector, though when parsing a `.json` file, you can have `parsebib` convert all values to strings.

You can access the data in an entry's alist with `assoc` or `alist-get`, but in order to accommodate for the differences in the type of field names, you may want to use `assoc-string` instead, which generalises over symbols and strings and has a `case-fold` argument.

Another thing to note is that in BibTeX data, the type and key of an entry are stored in the entry's alist under `=type=` and `=key=`, while the same information is available in CSL-JSON data under `type` and `id`, respectively. While some of the core information on an entry is available in fields with the same name in both formats (esp. author, editor and title), most fields are named differently (i.e.., the `year` field in BibTeX corresponds to the `issued` field in CSL-JSON).

One last difference to note is that the general buffer-parsing functions, `parsebib-parse-bib-buffer` and `parsebib-parse-json-buffer` do not have the same type of return value. See the function descriptions below for details.

There is also a function `parsebib-parse` that takes a list of files and returns the entries in them. It has a slightly simplified interface, but it is format-agnostic: it handles both `.bib` and `.json` files.


## BibTeX / `biblatex` ##

Support for `.bib` files comes in two different APIs, a higher-level one that reads all items in one go, and a lower-level one that reads one item at a time. `Parsebib` supports `@Preamble`, `@String` and `@Comment` items, and obviously actual bibliographic entries.


### Returning entries for display ###

In order to return entries in a way that is suitable for display, `parsebib` can post-process field values while reading the contents of a `.bib` file. This post-processing involves a number of modifications. First, it removes the braces or double quotes around field values,  replaces newlines, tabs and form feeds with spaces, and collapses sequences of whitespace characters into a single space.

Furthermore, TeX markup is prettified: LaTeX commands for special characters are replaced with their (Unicode) representations (i.e. `\textdollar` is replaced with $, `\S` with §, `---` with —, etc.), LaTeX commands that have an obligatory argument are replaced with that argument, optional arguments and braces are removed. In addition, the arguments of `\textbf`, `\textit` and `\emph` are given text properties so that they display as bold or italic, and the argument of `\textsc` is converted to caps. (Optionally, the arguments of `\text{bf|it}` and `\emph` can be converted to Org or Markdown; see below for details.) LaTeX commands that have no obligatory argument, such as `\LaTeX`, are retained.

Post-processing also involves expanding `@String` abbreviations: abbreviations in field values (or `@String` definitions) are replaced with their definition, so that field values are (more or less) shown the way they would appear after processing with BibTeX / `biblatex`.

The `file`, `url` and `doi` fields are excluded from post-processing, because they usually don't contain any TeX code or `@String` abbreviations, and because modifying them may actually be harmful (e.g., replacing multiple spaces with a single space in a file name). You can exclude more fields from post-processing by adding them to the variable `parsebib-postprocessing-excluded-fields`. (Note that double quotes or braces around the values of these fields *are* removed.)

In addition to this post-processing, `parsebib` can resolve cross-references. This means that if an entry has a `crossref` field, fields in the cross-referenced entry that are not already part of the cross-referencing entry are added to it. Both BibTeX's (rather simplistic) inheritance rule and `biblatex`'s more sophisticated inheritance schema are supported. It is also possible to specify a custom inheritance schema.

Expanding `@Strings` and resolving cross-references can also be done across files, by passing the result of parsing one file as arguments when parsing the next file. Details are discussed below.

Note that if you wish to resolve cross-references, it is usually also necessary to expand `@String` abbreviations, because the `crossref` field may contain such an abbreviation. Resolving such a cross-reference will not work unless the abbreviation is expanded.

When parsing a bibliography file for display, one may not be interested in all the data of each entry. The higher-level API functions can therefore take a list of fields to be read and included in the results. Fields not in this list are ignored, except for the `=key=` and `=type=` fields, which are always included.

If you use this option and also want to resolve cross-references, you need to include the `crossref` field in the list of requested fields. Without it, `parsebib` is not able to determine which entries cross-reference another entry and no cross-references will be resolved. Also note that cross-referencing may add fields to an entry that are not on the list of requested fields. For example, in `biblatex`, the `booktitle` field of an `InBook` entry is linked to the `title` field of the cross-referenced `Book` entry. In such a case, if `title` is on the list  of requested fields, the `booktitle` field is added to the cross-referencing entry, even if `booktitle` is not on the list of requested fields.


### Higher-level API ###

The higher-level API consists of functions that read and return all items of a specific type in the current buffer. They do not move point. Note that the arguments in these functions (except in `parsebib-expand-xrefs`) are keyword arguments.


#### `parsebib-collect-bib-entries (&key entries strings inheritance fields)` ####

Collect all entries in the current buffer and return them as a hash table, where the keys correspond to the BibTeX keys and the values are alists consisting of `(<field> . <value>)` pairs of the relevant entry. In this alist, the BibTeX key and the entry type are stored under `=key=` and `=type=`, respectively. Note that both `<field>` and `<value>` are strings. 

The argument `entries` can be used to pass a (possibly non-empty) hash table in which the entries are stored. This can be used to combine multiple `.bib` files into a single hash table, or to update an existing hash table by rereading its `.bib` file.

If the argument `strings` is present, `@String` abbreviations are expanded. `strings` should be a hash table of `@String` definitions as returned by `parsebib-collect-strings`. In addition, sequences of whitespace (space, tab, newline) are collapsed into a single space, field values are unquoted (i.e., the double quotes or braces around them are removed), and TeX markup is prettified or removed. Note that @String
expansion, collapsing of whitespace and prettifying TeX markup
are not applied to fields listed in `parsebib-postprocessing-excluded-fields`, but unquoting is.

If the argument `inheritance` is present, cross-references among entries are resolved. It can be `t`, in which case the file-local or global value of `bibtex-dialect` is used to determine which inheritance schema is used. It can also be one of the symbols `BibTeX` or `biblatex`, or it can be a custom inheritance schema. Note that cross-references are resolved against the entries that appear in the buffer *above* the current entry, and also against the entries in the hash table `entries`.

The argument `fields` is a list of names of the fields that should be included in the entries returned. Fields not in this list are ignored (except `=type=` and `=key=`, which are always included). Note that the field names should be strings; comparison is case-insensitive.


#### `parsebib-collect-strings (&key strings expand-strings)` ####

Collect all `@string` definitions in the current buffer and return them as a hash table. The argument `strings` can be used to provide a hash table to store the definitions in. If it is `nil`, a new hash table is created.

The argument `expand-strings` is a boolean value. If non-nil, any abbreviations found in the string definitions are expanded against the `@string` definitions appearing earlier in the `.bib` file and against `@string` definitions in `strings`, if provided.


#### `parsebib-collect-preambles` ####

Collect all `@preamble` definitions in the current buffer and return them as a list.


#### `parsebib-collect-comments` ####

Collect all `@comments` in the current buffer and return them as a list.


#### `parsebib-find-bibtex-dialect` ####

Find and return the BibTeX dialect for the current buffer. The BibTeX dialect is either `BibTeX` or `biblatex` and can be defined in a local-variable block at the end of the file.


#### `parsebib-parse-bib-buffer (&keys entries strings expand-strings inheritance fields replace-TeX)` ####

Collect all BibTeX data in the current buffer. Return a five-element list:

    (<entries> <strings> <preambles> <comments> <BibTeX dialect>)

The `<entries>` and `<strings>` are hash tables, `<preambles>` and `<comments>` are lists, `<BibTeX dialect>` is a symbol (either `BibTeX` or `biblatex`) or `nil`.

If the arguments `entries` and `strings` are present, they should be hash tables with `equal` as the `:test` function. They are then used to store the entries and strings, respectively.

The argument `expand-strings` functions as the same-name argument in `parsebib-collect-strings`, and the arguments `inheritance` and `fields` function as the same-name arguments in `parsebib-collect-bib-entries`.

If `replace-TeX` in set, (La)TeX markup in field values is replaced with text that is more suitable for display. The variable `parsebib-TeX-markup-replace-alist` determines what exactly is replaced. This variable can be `let`-bound around calls to the parsing functions, but note that its value is construed on the basis of the variables  `parsebib-TeX-command-relacement-alist`, `parsebib-TeX-accent-replacement-alist` or `parsebib-TeX-literal-replacement-alist`, so you may want to customise those instead. See their doc strings and the doc string of `parsebib-TeX-markup-replacement-alist` for details.

Note that `parsebib-parse-bib-buffer` only makes one pass through the buffer. It is therefore a bit faster than calling all the `parsebib-collect-*` functions above in a row, since that would require making four passes through the buffer.


#### `parsebib-postprocessing-excluded-fields` ####

This variable is set to a list of fields that should always be returned as-is, without any post-processing. The default value of this variable is  `("file" "url" "doi")`, though you can customise it by `let`-binding it around calls to the higher-level parse functions.

Note that double quotes or braces around field values *are* removed for fields listed in this variable.


#### `parsebib-TeX-cleanup-target` ####

This variable controls how `parsebib-clean-TeX-markup` the LaTeX font commands `\text{bf|it}` and `\emph`. If the variable is set to the symbol `display`, which is the default, these commands are replaced with their arguments augmented with a font property `bold` or `italic`. If this variable is set to `org` or `markdown`, they are instead converted to the appropriate markup.

#### `parsebib-expand-xrefs (entries inheritance)` ####

Expand cross-references in `entries` according to inheritance schema `inheritance`. `entries` should be a hash table as returned by `parsebib-collect-bib-entries`. Each entry with a `crossref` field is expanded as described above. The results are stored in the hash table `entries` again, the return value of this function is always `nil`.

This function can be useful if you use the lower-level API to parse `.bib` files, because in that case, resolving cross-references cannot be done while reading entries.


### Lower-level API ###

The lower-level API consists of functions that do the actual reading of a BibTeX item. Unlike the higher-level API, the functions here are dependent on the position of `point`. They are meant to be used in a `while` loop in which `parsebib-find-next-item` is used to move `point` to the next item and then use one of the `parsebib-read-*` functions to read the contents of the item.

All functions here take an optional position argument, which is the position in the buffer from which they should start reading. The default value is `(point)`.


#### `parsebib-find-next-item ()` ####

Find the first BibTeX item following point, where an item is either a BibTeX entry, or a `@Preamble`, `@String`, or `@Comment`. This function returns the item's type as a string, i.e., either `"preamble"`, `"string"`, or `"comment"`, or the entry type. Note that the `@` is *not* part of the returned string. This function moves point into the correct position to start reading the actual contents of the item, which is done by one of the following functions.


#### `parsebib-read-entry (&optional fields strings replace-TeX)` ####
#### `parsebib-read-string (&optional strings)` ####
#### `parsebib-read-preamble ()` ####
#### `parsebib-read-comment ()` ####

These functions do what their names suggest: read one single item of the type specified. `parsebib-read-entry` takes an optional argument `fields`, which is a list of names of the fields that should be included in the entries returned. Fields not in this list are ignored (except for `=type=` and `=key=`, which are always included). Note that the field names should be strings; comparison is case-insensitive.

`parsebib-read-entry` also takes an optional  argument `strings`, which is a hash table of `@String` definitions. When provided, abbreviations in the field values of the entry are expanded. In addition, when a `strings` argument is provided, newlines and other whitespace characters in the field value are replaced with a space, and sequences of whitespace are collapsed into a single space, and the braces or quotes around the field value are removed.

The last argument, `replace-TeX`, when non-nil, tells `parsebib-read-entry` to prettify TeX markup in the field value. Details are discussed [above](#returning-entries-for-display).

The reading functions return the contents of the item they read: `parsebib-read-preamble` and `parsebib-read-comment` return the text as a string. `parsebib-read-string` returns a cons cell of the form `(<abbrev> . <string>)`, and `parsebib-read-entry` returns the entry as an alist of `(<field> . <value>)` pairs. This alist contains entries for the entry key and the type under the keys `"=key="` and `"=type="`, respectively.

Note that all `parsebib-read*` functions move point to the end of the entry. If they cannot parse the item at point, they return an error of type `parsebib-error`, which can be captured, if necessary.


#### parsebib-clean-TeX-markup (string) ####

Apply all replacements in `parsebib-TeX-markup-replace-alist` to `string`. Note that this function ignores the value of `parsebib-postprocessing-excluded-fields`, because it just sees the string itself, not which field `string` comes from. 


## CSL-JSON ##

The support for CSL-JSON files comprises just one function: `parsebib-parse-json-buffer`. The actual parsing of the JSON data is performed by Emacs itself, either by the native JSON parsing routines (starting with Emacs 27.1, if available), or the built-in Elisp library `json.el`. `Parsebib` makes sure that the data is returned in a format that is similar to what is returned for `.bib` files.


### Returning entries for display ###

When returning entries in a form that is suitable for display, the most important issue in CSL-JSON files is the fact that certain fields do not have string values. For example, name fields (`author`, `editor`, etc.) and date fields (`issued`, `submitted` etc.) are JSON arrays. Parsebib can convert these to strings if requested.

As with `.bib` files, it is possible to have `parsebib` only return specific fields when reading `.json` files. Here, too, the fields that identify an entry, i.e., `id` and `type`, are always included and do not need to be requested explicitly.


#### `parsebib-parse-json-buffer (&key entries stringify year-only fields)` ####

Collect all CSL-JSON data in the current buffer and return the result. The return value is a hash table, where the keys correspond to the identifiers of the entries and the values are alists consisting of `(<field> . <value>)` pairs of the relevant entry. In this alist, the identifier is stored under the key `id` and the entry type is stored under `type`. `<field>` is a symbol, while `<value>` can be a string, a vector (array) or another alist.

The argument `entries` can be used to pass a (possibly non-empty) hash table in which the entries are stored. This can be used to combine multiple `.json` files into a single hash table, or to update an existing hash table by rereading its `.json` file.

Some field values in CSL-JSON are not strings. These are primarily name and date fields, which in CSL-JSON are represented as JSON objects. The argument `stringify` determines how they are returned. When `stringify` is set to `nil`, they are returned as alists; with `stringify` set to `t`, they are converted to strings.

The argument `year-only` controls the way dates are converted to strings. If it is non-`nil`, only the year part is returned. This argument only takes effect if `stringify` is set to `t`. See below for details.

The way values are converted to strings can be customised to some extent by the use of certain special variables, discussed below.

The argument `fields` is a list of names of the fields that should be included in the entries returned. Fields not in this list are ignored (except `type` and `id`, which are always included). Note that the field names should be symbols; comparison is case-sensitive.

Note that all arguments in this function are keyword arguments.


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

Field values that are arrays are converted to a string using the value of this variable as a separator. Currently (CSL-JSON v1.0), this only applies to the `categories` field, which is an array of strings. The default value of this variable is `", "` (note the space). It can be `let`-bound like the variables above.


## General API ##

### `parsebib-parse (files &key entries strings (display t) fields)` ###

Parse a bibliography file or list of files and return the entries in them. This function can be used for both `.bib` and for `.json` files, and also for a combination thereof. It returns all entries in all files in a single hash table.

This is a high-level function meant for retrieving bibliographic entries in such a way that they can be shown to a user. It is not possible to retrieve the `@Preamble` or `@Comment`s in a `.bib` file using this function. Use `parsebib-parse-bib-buffer` or one of the other functions for that.

`parsebib-parse` basically just calls `parsebib-parse-bib-buffer` or `parsebib-parse-json-buffer` as appropriate and passes its arguments on to those functions. The argument `entries` is passed to both, as is `fields`. The field names in `fields` need to be strings, regardless of the file format, though. `parsebib-parse` converts the strings to symbols when it parses a `.json` file. The `strings` argument is only passed to `parsebib-parse-bib-buffer`, since there are obviously no `@String`s in a `.json` file.

The `display` argument controls the way in which the entry data is returned. By default, it returns the data in a way that is suitable for display. For `.bib` files, this means that `@String` abbreviations are expanded, cross-references are resolved and TeX markup in field values is removed or replaced with Unicode characters. For `.json` files, it means that fields are returned as strings and that month and day parts in date fields are ignored.

See the doc strings of `parsebib-parse`, `parsebib-parse-bib-buffer` and `parsebib-parse-json-buffer` for details on the meaning of `display`.


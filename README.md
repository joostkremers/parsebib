Parsebib
=======

(c) 2014-2017 Joost Kremers

`Parsebib` is an Elisp library for reading `.bib` files. It provides two different APIs, a higher-level one that reads all items in one go, and a lower-level one that reads one item at a time. Supported items are `@Preamble`, `@String` and `@Comment` items, and obviously actual bibliographic entries.

Both APIs parse the current buffer. If you wish to combine multiple `.bib` files, you need to parse each separately.


Higher-level API
----------------

The higher-level API consists of functions that read and return all items of a specific type in the current buffer. They do not move point.


`parsebib-collect-entries (&optional hash strings)` 

Collect all entries in the current buffer and returns them as a hash table, where the keys correspond to the BibTeX keys and the values are alists consisting of `(<field> . <value>)` pairs of the relevant entries. In this alist, the BibTeX key and the entry type are stored under `=key=` and `=type=`, respectively.

The variable `hash` can be used to pass a hash table in which the entries are stored. This can be used to combine multiple `.bib` files into a single hash table, or to update an existing hash table by rereading its `.bib` file. If an entry is read from the buffer that has the same key as an entry in `hash`, the new entry overrides the old one.

The variable `strings` is a hash table of `@string` definitions, where the keys are the `@string` abbreviations and the values their expansions. If the variable `strings` is present, abbreviations occurring in the field values of the entries being read are expanded. Furthermore, the (outer) braces or double quotes are removed from field values.


`parsebib-collect-strings (&optional hash expand-strings)`

Collect all `@string` definitions in the current buffer. Again, the variable `hash` can be used to provide a hash table to store the definitions in. If it is `nil`, a new hash table is created and returned.

The argument `expand-strings` is a boolean value. If non-nil, any abbreviations found in the string expansions are expanded. You do not need to pass a hash table to the function for this to work. Every `@string` definition is added to the hash table as soon as it is read, which means that a `@string` definition can use an expansion defined earlier in the same file.


`parsebib-collect-preambles`

Collect all `@preamble` definitions in the current buffer and return them as a list.


`parsebib-collect-comments`

Collect all `@comments` in the current buffer and return them as a list.


`parsebib-find-bibtex-dialect`

Find and return the BibTeX dialect for the current buffer. The BibTeX dialect is either `BibTeX` or `biblatex` and can be defined in a local-variable block at the end of the file.


`parsebib-parse-buffer (&optional entries-hash strings-hash expand-strings)`

Collect all BibTeX data in the current buffer. Return a five-element list:

    (<entries> <strings> <preambles> <comments>, <BibTeX dialect>)

The `<entries>` and `<strings>` are hash tables, `<preambles>` and `<comments>` are lists, `<BibTeX dialect>` is a symbol (either `BibTeX` or `biblatex`).

The arguments `entries-hash` and `strings-hash` can be passed to store the entries and strings, respectively, in the same manner described above. The argument `expand-strings` is a boolean and has the same effect as the same-name argument in `parsebib-collect-strings`.

Note that `parsebib-parse-buffer` only makes one pass through the buffer. It should therefore be a bit faster that calling all the `parsebib-collect-*` functions above in a row, since that would require making four passes through the buffer.


Lower-level API
---------------

The lower-level API consists of functions that do the actual reading of a BibTeX item. Unlike the higher-level API, the functions here are dependent on the position of point. They are designed in such a way that calling them multiple times in succession will yield the contents of the entire `.bib` file. All functions here take an optional position argument, which is the position in the buffer from which they should start reading. In each function, the default value is `(point)`.

`parsebib-find-next-item (&optional pos)`

Find the first BibTeX item following point, where an item is either an entry, or a `@Preamble`, `@String`, or `@Comment`. This function returns the item's type as a string, i.e., either `"preamble"`, `"string"`, or `"comment"`, or the entry type. Note that the `@` is *not* part of the returned string. This function moves point into the correct position to start reading the actual contents of the item, which is done by one of the following functions.

`parsebib-read-string` (&optional pos strings)\
`parsebib-read-entry` (type &optional pos strings)\
`parsebib-read-preamble` (&optional pos)\
`parsebib-read-comment` (&optional pos)

These functions do what their names suggest: read one single item of the type specified. Each takes the `pos` argument just mentioned. In addition, `parsebib-read-string` and `parsebiib-read-entry` take an extra argument, a hash table of `@string` definitions. When provided, abbreviations in the `@string` expansions or in field values are expanded. Furthermore, the outermost braces or double quotes are removed. Note that `parsebib-read-entry` takes the entry type (as returned by `parsebib-find-next-entry`) as argument.

The reading functions return the contents of the item they read: `parsebib-read-preamble` and `parsebib-read-comment` return the text as a string. `parsebib-read-string` returns a cons cell of the form `(<abbrev> . <string>)`, and `parsebib-read-entry` returns the entry as an alist of `(<field> . <value>)` pairs. The alist contains an element with the key `=type=`, which holds the entry type, and an element with the key `=key=`, which holds the entry key. All functions move point to the end of the entry.

The reading functions return `nil` if they do not find the element they should be reading at the line point is on. Point is nonetheless moved, however. Similarly, `parsebib-find-next-item` returns `nil` if it finds no next entry, leaving point at the end of the buffer. Additionally, it will signal an error of type `parsebib-entry-type-error` if it finds something that it deems to be an invalid item name. What is considered to be a valid name is determined by the regexp `parsebib-bibtex-identifier`, which is set to `"[^^\"@\\&$#%',={}() \t\n\f]*"`, meaning that any string not containing whitespace or any of the characters `^"@\&$#%',={}()` is considered a valid identifier.

There is one additional function: `parsebib-find-bibtex-dialect`. This function looks for a local variable block in a `@Comment` and checks if the variable `bibtex-dialect` is set. If it is, it returns the value that is set (as a symbol). The value should be one of the elements in `bibtex-dialect-list`, i.e., by default one of `(BibTeX biblatex)`.

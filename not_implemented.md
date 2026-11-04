# Non-Implemented PostScript Features

This document catalogues all PostScript operators, syntax constructs, and subsystems present in the test suites (`test/phase4_testfiles/` and `test/ps_test/`) that are not implemented, along with the test files that use them.

---

## Operations by Category

### A. Virtual Memory (VM) Operations (`save`, `restore`)

PostScript Level 1/2 includes a virtual memory management subsystem that creates state snapshots (`save`) and restores the VM to previous states (`restore`), rolling back composite object mutations and dictionary bindings.

* **Operators**:
  * `save`: `- -> saveobj` — Creates a VM snapshot and returns a save object.
  * `restore`: `saveobj -> -` — Rolls back memory allocations and dictionary states.
* **Test Files**:
  * `test/phase4_testfiles/VMops.ps`
  * `test/phase4_testfiles/memops.ps`
  * `test/phase4_testfiles/test1.ps`
  * `test/phase4_testfiles/typetest.ps`
  * `test/ps_test/VMops.ps`
  * `test/ps_test/memops.ps`
  * `test/ps_test/test1.ps`

---

### B. File System & Stream I/O Operations

PostScript provides file objects and stream operations for reading, writing, and tokenizing arbitrary files from the host file system.

* **Operators**:
  * `file`: `filename access file -> file` — Opens a file stream with mode `(r)` or `(w)`.
  * `closefile`: `file closefile -> -` — Closes an open file stream.
  * `write`: `file int write -> -` — Writes a byte to an open file.
  * `read`: `file read -> file int true` | `file false` — Reads a byte from an open file.
  * `token` (file variant): `file token -> token true` | `false` — Reads and parses the next token directly from a file stream.
* **Test Files**:
  * `test/phase4_testfiles/fileops.ps`
  * `test/phase4_testfiles/tokentest.ps`
  * `test/phase4_testfiles/typetest.ps`
  * `test/ps_test/fileops.ps`

---

### C. Type, Attribute, and Conversion (TAC) Subsystem

PostScript supports dynamic type reflection and internal object attribute manipulation (`executable` vs `literal`).

* **Operators**:
  * `type`: `any type -> nametype` — Returns the internal type name (e.g. `/integertype`, `/arraytype`, `/dicttype`, `/marktype`, `/operatortype`, `/savetype`, `/filetype`).
  * `cvlit`: `any cvlit -> any` — Clears the executable attribute of an object, making it literal.
  * `cvx`: `any cvx -> any` — Sets the executable attribute of an object, making it executable.
  * `xcheck`: `any xcheck -> bool` — Tests whether an object has the executable attribute.
  * `cvi`: `num|str cvi -> int` — Converts a real number or numeric string to an integer.
  * `cvr`: `num|str cvr -> real` — Converts an integer or numeric string to a real number.
  * `cvn`: `str cvn -> name` — Converts a string to a literal or executable name object.
  * `cvrs`: `num radix str cvrs -> substring` — Converts a number to string with a given radix (base 2..36).
* **Test Files**:
  * `test/phase4_testfiles/TACops.ps`
  * `test/phase4_testfiles/tac.ps`
  * `test/phase4_testfiles/tac2.ps`
  * `test/phase4_testfiles/tac3.ps`
  * `test/phase4_testfiles/tac4.ps`
  * `test/phase4_testfiles/typetest.ps`
  * `test/phase4_testfiles/factorial.ps`

---

### D. String Searching, Tokenizing & In-Place Mutation

In PostScript, strings can be treated as fixed-length mutable byte arrays with pattern searching and self-tokenization.

* **Operators**:
  * `anchorsearch`: `string seek anchorsearch -> post match true` | `string false` — Determines if a string begins with a substring.
  * `search`: `string seek search -> post match pre true` | `string false` — Searches for the first occurrence of a substring.
  * `token` (string variant): `string token -> post token true` | `false` — Scans the next token from a string buffer.
  * In-place character mutation via `put`: `string index int put -> -` — Modifies character byte at index.
* **Test Files**:
  * `test/phase4_testfiles/as.ps`
  * `test/phase4_testfiles/stringops.ps`
  * `test/phase4_testfiles/stringops2.ps`
  * `test/phase4_testfiles/token1.ps`
  * `test/phase4_testfiles/tokens.ps`

---

### E. Advanced Dictionary Stack Reflection & Cross-Scope Mutation

Standard PostScript provides operators to inspect the dictionary stack as an array, count active scopes, mutate outer scopes, or manipulate standard system dictionary objects.

* **Operators & Syntax**:
  * `countdictstack`: `- -> int` — Returns the total count of dictionaries on the dictionary stack.
  * `cleardictstack`: `- -> -` — Pops all user dictionaries down to the root dictionary.
  * `dictstack`: `array dictstack -> subarray` — Copies the current dictionary stack into an array.
  * `currentdict`: `- -> dict` — Pushes the topmost dictionary onto the operand stack.
  * `maxlength`: `dict maxlength -> int` — Returns the maximum capacity of a dictionary.
  * `where`: `key where -> dict true` | `false` — Searches the dict stack and returns the dictionary containing `key`.
  * `store`: `key val store -> -` — Overwrites `key` in the nearest dictionary where it is defined, or in `currentdict`.
  * `undef`: `dict key undef -> -` — Removes a key-value binding from a dictionary.
  * `systemdict` & `userdict`: Predefined standard system dictionary names.
  * `<<` and `>>`: Dictionary literal syntax (e.g. `<< /a 1 /b 2 >>`).
* **Test Files**:
  * `test/phase4_testfiles/dictops.ps`
  * `test/phase4_testfiles/dictops2.ps`
  * `test/phase4_testfiles/green_dictionaryTest.ps`
  * `test/phase4_testfiles/psprog6.ps`
  * `test/phase4_testfiles/psprog21.ps`
  * `test/phase4_testfiles/factorial.ps`
  * `test/ps_test/dictops.ps`
  * `test/ps_test/green_dictionaryTest.ps`
  * `test/ps_test/psprog6.ps`
  * `test/ps_test/psprog21.ps`

---

### F. Bitwise Integer Operations & Shifts

PostScript overloads logical operators (`and`, `or`, `not`, `xor`) to perform bitwise logic when operands are integers, and provides `bitshift`.

* **Operators**:
  * `bitshift`: `int1 shift bitshift -> int2` — Bitwise left shift (positive `shift`) or right shift (negative `shift`).
  * Bitwise `and`: `int1 int2 and -> int3` — Bitwise AND of two integers.
  * Bitwise `or`: `int1 int2 or -> int3` — Bitwise OR of two integers.
  * Bitwise `xor`: `int1 int2 xor -> int3` — Bitwise XOR of two integers.
  * Bitwise `not`: `int1 not -> int2` — Bitwise bit inversion of an integer.
* **Test Files**:
  * `test/phase4_testfiles/bitshiftTest.ps`
  * `test/phase4_testfiles/andTest.ps`
  * `test/phase4_testfiles/orTest.ps`
  * `test/phase4_testfiles/xorTest.ps`

---

### G. Mark-Based Stack Delimiters

PostScript defines a special `mark` object used to delimit variable-length collections of objects on the operand stack.

* **Operators**:
  * `mark`: `- -> mark` — Pushes a mark object onto the stack (also synonymous with `[`).
  * `counttomark`: `mark obj_1 ... obj_n counttomark -> mark obj_1 ... obj_n n` — Counts items above the mark.
  * `cleartomark`: `mark obj_1 ... obj_n cleartomark -> -` — Pops all items down through the mark.
* **Test Files**:
  * `test/phase4_testfiles/opstackops.ps`
  * `test/phase4_testfiles/green_operandTest.ps`
  * `test/phase4_testfiles/tac.ps`
  * `test/phase4_testfiles/tac2.ps`
  * `test/phase4_testfiles/tac3.ps`
  * `test/phase4_testfiles/tac4.ps`
  * `test/phase4_testfiles/typetest.ps`
  * `test/ps_test/opstackops.ps`
  * `test/ps_test/green_operandTest.ps`

---

### H. Pseudo-Random Number Generation (PRNG)

* **Operators**:
  * `rand`: `- -> int` — Returns a pseudo-random integer.
  * `srand`: `int srand -> -` — Seeds the random number generator.
  * `rrand`: `- -> seed` — Returns the current random seed integer.
* **Test Files**:
  * `test/phase4_testfiles/mathops.ps`
  * `test/ps_test/mathops.ps`

---

### I. Advanced Array Slicing & Stack Marshalling

Operators for sub-interval array slicing and bulk stack dumping.

* **Operators**:
  * `aload`: `array aload -> a_0 ... a_{n-1} array` — Unpacks all array elements onto the stack.
  * `astore`: `any_0 ... any_{n-1} array astore -> array` — Pops elements into an array.
  * `getinterval`: `array index count getinterval -> subarray` — Creates a sub-array view.
  * `putinterval`: `array1 index array2 putinterval -> -` — Overwrites a slice of an array with another array.
* **Test Files**:
  * `test/phase4_testfiles/arrayops.ps`

---

### J. Special Literals & Extended Number Formats

* **Features**:
  * `null`: The PostScript literal `null` object (`PSNull`).
    * *Tested in*: `test/phase4_testfiles/functional4.ps`, `test/phase4_testfiles/typetest.ps`, `test/ps_test/functional4.ps`.
  * Radix numbers: Numbers written as `base#number` (e.g., `16#FF`, `8#77`).
    * *Tested in*: `test/phase4_testfiles/mathops.ps`.
  * Scientific notation: Numbers with decimal exponents (e.g., `1e5`, `1.0E-5`, `-1.234e2`).
    * *Tested in*: `test/phase4_testfiles/mathops.ps`, `test/phase4_testfiles/relops.ps`, `test/ps_test/mathops.ps`, `test/ps_test/relops.ps`.

---

## Summary Table

| Category | Operator / Syntax | Description | Test Files in Repo |
| :--- | :--- | :--- | :--- |
| **Virtual Memory** | `save` | Snapshot interpreter VM state | `VMops.ps`, `memops.ps`, `test1.ps`, `typetest.ps` |
| **Virtual Memory** | `restore` | Restore VM to snapshot | `VMops.ps`, `memops.ps`, `test1.ps` |
| **File I/O** | `file` | Open file stream | `fileops.ps`, `tokentest.ps`, `typetest.ps` |
| **File I/O** | `closefile` | Close file stream | `fileops.ps` |
| **File I/O** | `write` | Write byte to file | `fileops.ps` |
| **File I/O** | `read` | Read byte from file | `fileops.ps` |
| **File I/O** | `token` (file) | Tokenize from file stream | `tokentest.ps` |
| **TAC** | `type` | Return type name of object | `TACops.ps`, `tac.ps`, `tac2.ps`, `tac3.ps`, `tac4.ps`, `typetest.ps` |
| **TAC** | `cvlit` | Make object literal | `TACops.ps`, `tac.ps`, `tac3.ps`, `tac4.ps` |
| **TAC** | `cvx` | Make object executable | `TACops.ps`, `tac.ps`, `tac3.ps`, `factorial.ps` |
| **TAC** | `xcheck` | Test if object is executable | `TACops.ps`, `tac.ps`, `tac3.ps`, `tac4.ps` |
| **TAC** | `cvi` | Convert to integer | `TACops.ps`, `tac.ps`, `tac3.ps`, `tac4.ps` |
| **TAC** | `cvr` | Convert to real | `TACops.ps`, `tac.ps`, `tac3.ps`, `tac4.ps` |
| **TAC** | `cvn` | Convert to name | `TACops.ps`, `tac.ps`, `tac3.ps` |
| **TAC** | `cvrs` | Convert number to string with radix | `tac.ps`, `tac2.ps`, `tac3.ps` |
| **String** | `anchorsearch`| Check string prefix match | `as.ps`, `stringops.ps` |
| **String** | `search` | Find substring match | `as.ps`, `stringops.ps` |
| **String** | `token` (string)| Tokenize next token from string | `stringops.ps`, `token1.ps`, `tokens.ps` |
| **Dict Stack** | `countdictstack`| Count dictionaries on dict stack | `dictops.ps`, `dictops2.ps`, `green_dictionaryTest.ps`, `psprog21.ps` |
| **Dict Stack** | `cleardictstack`| Pop down to base dictionaries | `dictops.ps`, `dictops2.ps`, `green_dictionaryTest.ps` |
| **Dict Stack** | `dictstack` | Dump dict stack into array | `dictops.ps`, `dictops2.ps`, `psprog21.ps` |
| **Dict Stack** | `currentdict` | Push current top dictionary | `dictops.ps`, `dictops2.ps`, `typetest.ps` |
| **Dict Stack** | `where` | Find dict containing key | `dictops.ps`, `dictops2.ps` |
| **Dict Stack** | `store` | Set value in nearest defining dict | `dictops.ps` |
| **Dict Stack** | `undef` | Remove key from dictionary | `dictops.ps`, `dictops2.ps` |
| **Dict Stack** | `maxlength` | Return dictionary capacity | `dictops.ps`, `dictops2.ps` |
| **Dict Stack** | `systemdict` | System dictionary symbol | `psprog6.ps` |
| **Dict Stack** | `userdict` | User dictionary symbol | `psprog6.ps` |
| **Dict Syntax** | `<<` and `>>` | Dictionary literal syntax | `dictops.ps`, `dictops2.ps`, `green_dictionaryTest.ps`, `psprog11.ps`, `psprog20.ps`, `factorial.ps` |
| **Bitwise** | `bitshift` | Arithmetic/bitwise bit shift | `bitshiftTest.ps` |
| **Bitwise** | `and`, `or`, `xor` (int) | Integer bitwise logic | `andTest.ps`, `orTest.ps`, `xorTest.ps` |
| **Mark Stack** | `mark` | Push mark token | `opstackops.ps`, `green_operandTest.ps`, `tac.ps`, `tac2.ps`, `typetest.ps` |
| **Mark Stack** | `counttomark` | Count items above mark | `opstackops.ps`, `green_operandTest.ps` |
| **Mark Stack** | `cleartomark` | Clear stack down to mark | `opstackops.ps`, `green_operandTest.ps` |
| **PRNG** | `rand`, `srand`, `rrand` | Random number generator suite | `mathops.ps` |
| **Array Slice** | `aload`, `astore` | Bulk stack dump/load | `arrayops.ps` |
| **Array Slice** | `getinterval`, `putinterval` | Sub-array slicing | `arrayops.ps` |
| **Literals** | `null` | Null literal | `functional4.ps`, `typetest.ps` |
| **Lexer** | `base#number` | Radix number literal | `mathops.ps` |
| **Lexer** | `1e5`, `1.0E-5` | Scientific float notation | `mathops.ps`, `relops.ps` |

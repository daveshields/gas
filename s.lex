* copyright 1987-2012 robert b. k. dewar and mark emmer.
*
* copyright 2012-2015 david shields
*
* this file is part of macro spitbol.
*
*     macro spitbol is free software: you can redistribute it and/or modify
*     it under the terms of the gnu general public license as published by
*     the free software foundation, either version 2 of the license, or
*     (at your option) any later version.
*
*     macro spitbol is distributed in the hope that it will be useful,
*     but without any warranty; without even the implied warranty of
*     merchantability or fitness for a particular purpose.  see the
*     gnu general public license for more details.
*
*     you should have received a copy of the gnu general public license
*     along with macro spitbol.  if not, see <http://www.gnu.org/licenses/>.
*
*      spitbol conditional assembly symbols for use by token.spt
*      ---------------------------------------------------------
*
*      this file of conditional symbols will override the conditional
*      definitions contained in the spitbol minimal file.   in addition,
*      lines beginning with ">" are treated as spitbol statements and
*      immediately executed.
*
*      for linux spitbol-x86
*
*      in the spitbol translator, the following conditional
*      assembly symbols are referred to. to incorporate the
*      features referred to, the minimal source should be
*      prefaced by suitable conditional assembly symbol
*      definitions.
*      in all cases it is permissible to default the definitions
*      in which case the additional features will be omitted
*      from the target code.
*
*
*                            conditional options
*                            since .undef not allowed if symbol not
*                            defined, a full comment line indicates
*                            symbol initially not defined.
*
*      .caex                 define to allow up arrow for exponentiation
*      .cavt                 define to include vertical tab
*      .ccmc                 define to include syscm function
*      .ceng                 define to include engine features
*      .cnci                 define to enable sysci routine
*      .cncr                 define to enable syscr routine
*      .cnex                 define to omit exit() code.
*      .cnld                 define to omit load() code.
*      .cnpf                 define to omit profile stuff
*def   .cnra                 define to omit all real arithmetic
*      .cnsr                 define to omit sort, rsort
*      .crpp                 define if return points have odd parity
*      .cs16                 define to initialize stlim to 32767
*      .culc                 define to include &case (lc names)
*      .cust                 define to include set() code
*      .cusr                 define to have set() use real values
*                             (must also #define setreal 1 in systype.h)
*
{{ttl{27,l i c e n s e -- software license for this program{{{{93
*     copyright 1983-2012 robert b. k. dewar
*     copyright 2012-2015 david shields
*     this file is part of macro spitbol.
*     macro spitbol is free software: you can redistribute it and/or modify
*     it under the terms of the gnu general public license as published by
*     the free software foundation, either version 2 of the license, or
*     (at your option) any later version.
*     macro spitbol is distributed in the hope that it will be useful,
*     but without any warranty; without even the implied warranty of
*     merchantability or fitness for a particular purpose.  see the
*     gnu general public license for more details.
*     you should have received a copy of the gnu general public license
*     along with macro spitbol.  if not, see <http://www.gnu.org/licenses/>.
{{ttl{27,s p i t b o l -- notes to implementors{{{{113
*      m a c r o   s p i t b o l     v e r s i o n   13.01
*      ---------------------------------------------------
*      date of release  -  january 2013
*      macro spitbol is maintained by
*           dr. david shields
*           260 garth rd apt 3h4
*           scarsdale, ny 10583
*      e-mail - thedaveshields at gmail dot com
*      version 3.7 was maintained by
*           mark emmer
*           catspaw, inc.
*           p.o. box 1123
*           salida, colorado 81021
*           u.s.a
*      e-mail - marke at snobol4 dot com
*      versions 2.6 through 3.4 were maintained by
*           dr. a. p. mccann (deceased)
*           department of computer studies
*           university of leeds
*           leeds ls2 9jt
*           england.
*      from 1979 through early 1983 a number of fixes and
*      enhancements were made by steve duff and robert goldberg.
{{ttl{27,s p i t b o l - revision history{{{{145
{{ejc{{{{{146
*      r e v i s i o n   h i s t o r y
*      -------------------------------
*      version 13.01 (january 2013, david shields)
*      this version has the same functionality as the previous release, but with
*      many internal code changes.
*      support for x86-64 has been added, but is not currently working.
*      the description of the minimal language formerly found here as comments
*      is now to be found in the file minimal-reference-manual.html
*      version 3.8 (june 2012, david shields)
*      --------------------------------------
*	       this version is very close to v3.7, with the same functionality.
*              the source is now maintained using git, so going forward
*              the detailed revision history will be recorded in the git
*              commit logs, not in this file.
*      version 3.6a to 3.7 (november 1, 1991, mark b. emmer)
*      -----------------------------------------------------
*
*      bugs fixed
*      ----------
*
*      b3.701  add btkwv and refined test at cdgvl+9 to prevent
*              variable names alphabet, lcase, ucase from being
*              pre-evaluated because of their associated
*              constant keywords.  the code
*                 alphabet = "abc"; output = size(alphabet)
*              returned zero because of pre-evaluation.
*      b3.702  delay binding to function block of fourth
*              argument to trace function.  this permits the
*              trace function to be invoked before the 4th
*              argument function is defined.  accomplished by
*              storing a vrblk pointer in trfnc, and fetching
*              its vrfnc entry later, in trxeq.
*      b3.703  references to keywords with constant pattern
*              values (&arb, &bal, etc.) did not work.  a wtb
*              instruction had been omitted at acs14+2.
*      b3.704  if a program employed the code function to
*              redefine a label that was the entry location of
*              a user-defined function, the function would
*              continue to jump to its old function body.  pfcod
*              in pfblk was pointing directly to the target code
*              block, instead of doing so indirectly through the
*              vrblk for the entry label.
*      b3.705  the test that required a label to be defined
*              before it could be used as the entry of a user-
*              defined function has been removed.  functions
*              may be defined even if the label is yet
*              undefined.
*      b3.706  after a compilation error in the code function,
*              the eval function produces spurrious errors.  the
*              code offset cwcof was not being reset to the
*              beginning of code block.  add line at err04+1 to
*              accomplish this reset.
*      b3.707  inconsistant tests with mxlen corrected.  several
*              places were testing with bge instead of bgt,
*              resulting in such anomalies as the statement
*                 &maxlngth = &maxlngth
*              failing.  since mxlen is guaranteed to be
*              strictly less than dnamb, it is permissible to
*              create objects of size mxlen.  bge changed to
*              bgt at locations
*                 s$arr+14, sar07+8, alobf+3, asg14+8, gtar6+10.
*      b3.708  exit(command string) was not loading ptr to fcb
*              chain into wb.  corrected at sext1.
*      b3.709  change patst to return non-string error for null
*              argument.  previously, break(), any(), etc., were
*              succeeding, contrary to the language definition.
*      b3.710  convert function with null second argument
*              crashed system by calling flstg with wa=0.  added
*              test at s$cnv, moved error 74 to separate erb at
*              scv29.
*      b3.711  leq(,) crashed system.  lcomp did not obey
*              minimal assumption that cmc opcode will always
*              be called with wa .gt. 0.  added test at lcmp1.
*      b3.712  modified line at sdf07+4 to use register wa
*              instead of wb.  this corrects problem of define
*              function with local variable list that begins
*              with comma-  define("f(x),l1,l2")
*      b3.713  erroneous plc on uninitialised r$cim in listr.
*      b3.714  erroneous call to flstg possible with null string
*              at sdat1.
*      b3.715  when copy function used with table argument, fix
*              problem at cop07.  when copying first teblk on a
*              chain, the pseudo-previous block pointer in xr
*              is pushed on the stack prior to calling alloc.
*              this is not a valid block pointer, as it points
*              within the tbblk.  if the subsequent alloc
*              invokes gbcol, the heap becomes scrambled.
*              recoded to save pointer to start of block, plus
*              offset in wb.
*      b3.716  at iop01, if gtvar triggered garbage collection
*              via alost, trap block in wc was not collected.
*              save wc on stack to make it collectable across
*              gtvar call.
*      b3.717  at asg10, allow case of variable with more than
*              one trblk, as happens with the following stmt -
*              output(.output, .output, filename).
*      b3.718  at senf1, trblk chain search was reloading chain
*              head, causing infinite loop if the desired trblk
*              was not the first on chain.  system crashed with
*              trace(.v1) output(.v2,.v1,file).
*      b3.719  prototype strings (define, load, data, etc.) were
*              allowing blank characters, producing bogus
*              variable names.
*      b3.720  the fact that iofcb destroyed register wc was not
*              documented.  b$efc conversion of file argument
*              never worked because wc and xt were destroyed by
*              call to iofcb.
*      b3.721  ioput left a trblk attached to filearg1 if sysio
*              failed.  subsequent use of this filearg1 variable
*              in another i/o call would crash system.
*      b3.722  add chk at evlp1 to catch recursive pattern error.
*      b3.723  allow -line to work properly within code function
*              by setting cmpln directly in cnc44.  if file name
*              absent, decrement scnpt to rescan terminator.
*      b3.724  when mxlen exceeds start of dynamic memory, round
*              it up to multiple of word size prior to storing
*              in dnamb at ini06.
*      b3.725  provide right padding of zero characters to any
*              string returned by an external function.
*      b3.726  reset flptr at bpf17 for undefined function
*              when evalx is evaluating an expression.
*      b3.727  modify code after read5 for outer nesting of
*              an execute-time compile of -include statement.
*              create a substring of remainder of original
*              code function argument string and return as
*              result of readr function
*      b3.728  the definition of the aov opcode is corrected.
*              formerly the definition specified that the branch
*              was to be taken if the result of the addition
*              exceeded cfp$m, implying a test for overflow
*              from signed addition.
*              however, address arithmetic must be unsigned to
*              allow for systems where the high order address
*              bit is set.  therefore, the test must be for
*              carry out of the high order bit, if the result
*              would exceed cfp$l.
*      b3.729  a label trace on the entry label for a function
*              was undetected, resulting in a transfer to
*              b$trt and subsequent crash.  see bpf08 for fix.
*      b3.730  pop first argument to substr if it is a buffer.
*      b3.731  pattern replacement with buffer subject returned
*              null string instead of new subject value.
*              changed to behave as if subject was a string.
*      b3.732  if convert function was called with a buffer
*              first argument and "buffer" second argument,
*              it would convert the buffer to a string, and
*              then back to a buffer.  this has be corrected
*              to simply return the first argument as the
*              function result.
*      b3.733  detect external function returning a null string
*              unconverted result at bef12, and jump to exnul.
*      b3.734  fix problem at ins04 when inserting zero length
*              string into buffer.  defend against invoking
*              mvc with a zero value in wa, which will cause
*              some implementations to wrap the counter.
*      b3.735  add overflow test for cos and sin to detect
*              out-of-range argument.
*      b3.736  fixed problem introduced with b3.727 not
*              restoring r$cim, scnpt and scnil after creating
*              substring.
*      b3.737  fixed tfind to place default value in newly
*              allocated teblk.
*      b3.738  added bl$p0 to p$nth entry point.  the expression
*              datatype(convert("","pattern")) would crash when
*              the dtype function uses the non-existant type
*              word preceding p$nth.
*      b3.739  bug at gtn35 in the case of overflow during cvm.
*              wb can be destroyed by cvm on some platforms.
*      b3.740  protect scontinue from usage in other than error
*              320 case.
*      b3.741  protect continue from usage following error
*              evaluating complex failure goto.
*
*
*      changes
*      -------
*
*      c3.701  add .culk conditional to include &lcase, &ucase.
*      c3.702  add -line nn "filename" control card.
*      c3.703  move .cnld conditional up in routine dffnc to
*              omit all tests for b$efc.
*      c3.704  add conditional .cicc to ignore unrecognized
*              control cards.
*      c3.705  add conditional .cnsc to omit string to numeric
*              conversion in sort.  the presence of this
*              conversion mode produces a sort result that is
*              dependent upon the order of input data.
*              for example, given input data "2", 5, "10",
*              string comparison yields "10" lt "2", but string
*              to integer conversion yields "2" lt 5 lt "10".
*      c3.706  add seventh return from syshs that allows callee
*              to return a string pointer and length.  this is
*              done to eliminate the need for the caller to have
*              an scblk big enough to accommodate long strings.
*      c3.707  add eighth return from syshs to force copy of
*              block pointed to by xr.
*      c3.708  made -copy a synonym for -include.
*      c3.709  add conditional .cbyt for statistics displayed
*              in bytes rather than words.
*      c3.710  dump null valued variables when dump = 3.  core
*              dump produced for dump = 4.
*      c3.711  restrict minimum value to which keyword maxlngth
*              can be set to 1,024 via new variable mnlen.
*      c3.712  add conditional symbol .cmth for extended math
*              functions- atan, chop, cos, exp, ln, sin, sqrt,
*              tan.  x**y and remdr(x,y) are extended to include
*              reals.
*      c3.713  add bit to syspp to set -print upon entry
*      c3.714  add conditional .csfn to track source file name
*              associated with each code block.
*      c3.715  add conditional .cinc for -include control card
*              feature.  the format of the card is
*                 -include "filename"
*              include control cards may be used during both the
*              initial compile and execute-time compile.  the
*              filename is saved in a table, and redundant
*              includes of that file are ignored.
*      c3.716  add conditional .csln to include source line
*              number in code blocks.  release current ccblk
*              after initial compile.
*      c3.717  changed rilen to 258 (from 120) to provide
*              uniform input line length when reading from
*              terminal or input.
*      c3.718  add additional exit to iofcb to distinguish
*              argument not convertable to string and argument
*              file not open.
*      c3.719  add fourth and fifth arguments to host function.
*      c3.720  add &compare keyword to control string
*              comparisons.
*      c3.721  setup pfdmp at iniy0 in case osint forced
*              &profile non-zero.
*      c3.722  add conditional symbol .caex to include up arrow
*              as synonym for exponentiation.
*      c3.723  add conditional .ccmc and external function syscm
*              to provide string comparison using collation
*              sequence other than strict ordering of character
*              codes (international compares).
*      c3.724  add conditional .cpol and external function syspl
*              to provide interactive control of spitbol
*              execution.
*      c3.725  add conditional symbol .cera and external
*              function sysea to provide advice of compilation
*              and runtime errors to osint.
*      c3.726  add cmpln, rdcln, rdnln to track source line
*              number.
*      c3.727  converted error messages to upper/lower case.
*      c3.728  add conditional .cgbc to external routine sysgc.
*              called at the start and end of garbage collection
*              to perform any needed notification to operating
*              system or user.
*      c3.729  modified last line of s$set from exnul to exint
*              so seek can return final file position after
*              seek.
*      c3.730  place mov xr,(xs) at s$rmd+4 to allow real second
*              arg to remdr.
*      c3.731  remove redundant bge xr,=cfp$u,scn07 at scn06+4
*      c3.732  change definition of cmc and trc such that only
*              xl must be cleared after operation.  note, this
*              change was subsequently voided.  cmc and trc must
*              clear both xl and xr, because utility routines
*              may preserve xl or xr on the stack, and the stack
*              is collectable by gbcol.
*      c3.733  remove most branches to exits and exixr.
*              instead, jump directly to next code word.
*      c3.734  add error 260 for array too large in gtarr.
*      c3.735  add conditional .cs32 to initialize stlim to
*              2147483647.
*      c3.736  add second argument to exit function, allowing
*              user to specify file name of load module being
*              written.  if omitted, osint will provide a
*              default name.
*      c3.737  add conditional .cspr to include spare locations
*              in working area.  these may be used in later bug
*              fixes without changing the size of the working
*              storage and obsoleting modules created by exit().
*              subsuently removed in c3.767.
*      c3.738  add r$cts to remember last string used to build
*              bit column in patst.
*      c3.739  change flstg to type e procedure instead of r.
*      c3.740  standardize on big-endian systems.  at the
*              implementors choice, the zgb opcode can also
*              perform a byte swap if necessary to achieve big-
*              endian byte ordering.  this is done so that
*              systems with similar word lengths will produce
*              the same hash code for strings, and hence the
*              same ordering for table entries.  the hashs
*              procedure has an additional zgb added to reorder
*              the length word.
*      c3.741  add conditional .csou to cause assignments to
*              output and terminal variables to be processed
*              through calls to sysou rather than through
*              listing buffer.  done to eliminate short record
*              lengths enforced by buffer size.  a code of 0 or
*              1 is passed to sysou instead of an fcblk.
*      c3.742  increased iniln, inils, rilen to 1024.
*      c3.743  add bit to syspp to set noerrors mode.
*      c3.744  add .ccmk conditional to include keyword compare
*              even if syscm is not being included.  done to
*              provide identical data regions in systems that
*              implement syscm and those which do not, so that
*              save files can be exchanged in the next release.
*      c3.745  add wc return parameter to sysil to allow
*              interface to inform spitbol if file about to be
*              read is a binary file.  if so, no blank trimming
*              occurs.
*      c3.746  fold load function argument types to upper case.
*      c3.747  add .cexp conditional to have sysex pop its
*              arguments.
*      c3.748  in stopr, do not attempt to display file name and
*              line number if stopping because of stack overflow
*              during garbage collection.  pointers to file name
*              table and code block are wrong.
*      c3.749  add bit to syspp to set case folding mode.
*      c3.750  add additional return from sysld if insufficient
*              memory to load/call external function.
*      c3.751  add additional returns from sysex if insufficient
*              memory or bad argument type.
*      c3.752  ignore leading and trailing blanks in arguments
*              within prototype strings to clear, data, define
*              and load.
*      c3.753  test for fatal error at err04 and abort if so.
*              force termination on stack overflow by setting
*              errft to 4 in stack overflow section.
*      c3.754  recode copy loop at srt14 to exchange usage of
*              registers xl and xr.  this permits use of the
*              mvw order instead of the explicit loop coding
*              previously employed.
*      c3.755  add .ceng conditional to include routines needed
*              by text processing engine. add routines enevs and
*              engts for use by engine or debugger.  copy xr to
*              xl around call to syspl to allow syspl to
*              trigger garbage collection.
*      c3.756  add &file, &lastfile, &line, &lastline keywords.
*              for now, line and lastline are maintained in the
*              same manner as stno and lastno, which adds over-
*              head to the statement initialization code.  a
*              possible change is to create a stmln procedure
*              that maps statement numbers to line numbers.
*              one simple strategy would be to sweep code blocks
*              in memory looking for the statement number and
*              extracting the line number from that code block.
*              such a procedure would also allow line numbers
*              (and file names) to be added to statement profile
*              reports.
*      c3.757  change sort to fail instead of producing error
*              message if argument table is null.  change sorta
*              to return failure.  add another return to gtarr
*              to distinguish null table from bad argument.
*      c3.758  create procedure prtmm to display memory usage
*              statistics, and call it when producing end-of-
*              run stats.
*      c3.759  add label scontinue to allow setexit to resume
*              execution exactly where it was interrupted.
*      c3.760  add snobol4 backspace function and conditional
*              .cbsp.
*      c3.761  add additional arguments to sysgc to assist
*              virtual memory managers.
*      c3.762  the method of converting a table to an array has
*              been revised.  previously, table elements were
*              copied to the result array in the order they were
*              encountered along the various hash chains.  this
*              appeared to the user as a random ordering.  how-
*              ever, spitbol/370 as well as sil snobol4 ordered
*              array elements according to their time of entry
*              into the table.  user programs that relied upon
*              this behavior malfunctioned when ported to macro
*              spitbol.
*              to remedy this, the conversion is performed in
*              three steps:
*              1. convert table to an array placing the address
*                 of each teblk in the array instead of the key
*                 and value.
*              2. sort the array of addresses.  this orders ele-
*                 ments by time of creation (ascending address).
*              3. scan the array, replacing addresses with the
*                 key and value from the referenced teblk.
*              the affected portions of the program are at s$cnv
*              and in gtarr, which now accepts an additional
*              argument specifying whether to place key/values
*              in the array or teblk addresses.
*      c3.763  if case-folding is active, fold the function name
*              provided to the load() function before passing it
*              to sysld.
*      c3.764  add sediment algorithm to garbage collector,
*              conditioned on .csed.
*      c3.765  add optimization to discard null statements and
*              statements which just have a constant subject
*              (see code at cmp12).
*      c3.766  rearranged order of initial objects in static
*              memory so that hash table is the last of the four
*              object created by initialization code.  this is
*              done so that the print buffer, gts work area, and
*              &alphabet keywords do not need to be saved in
*              any save file created by osint.  added routine to
*              initialize these structures.
*      c3.767  removed .cspr conditional and spare locations.
*      c3.768  added .crel conditional and extensive routines
*              (reloc et. al.) to perform relocation of data
*              in working section, static region, and dynamic
*              region after reload of a saved memory image.
*              routines relaj, relcr, and reloc are invoked
*              by osint after reloading a save file.
*              it is now possible to reload such an image even
*              if the spitbol compiler and its data structures
*              are reloaded to other addresses.  the working
*              section has been extensively rearranged to
*              accommodate the reloc procedure.
*      c3.769  zero r$ccb (interim ccblk ptr) in collect,
*              convert, eval, and exit functions to release
*              unneeded ccblk memory.
*      c3.770  add exit(4) and exit(-4) to allow execution to
*              continue after writing save file or load module.
*              revised sysxi interface to detect continuation
*              after performance of exit(4) or exit(-4) action.
*      c3.771  change filnm to preserve registers.
*      c3.772  addition of .cncr and syscr (real to string
*              system routine option).
*      c3.773  modified replace function to optimize usage
*              when second argument is &alphabet.  in this case,
*              the third argument can be used as the translate
*              table directly.
*      c3.774  modified conditionals for buffers and reals so
*              that their respective block codes are always
*              present, even if these data types are conditioned
*              out.  this provides consistent block code
*              numbering for external functions.
*      c3.775  modified alobf to test string length against
*              kvmxl instead of mxlen.  also, alobf was testing
*              total size of bfblk, instead of just string len.
*      c3.776  move utility routines source up to lie between
*              predefined snobol functions (s$xxx) routines and
*              utility procedures.  this was done to assist
*              translation on platforms such as apple macintosh
*              that use 15-bit offsets to store error exits (ppm
*              branches).  offsets to labels like exfal were
*              just too far away.  similarly, functions tfind,
*              tmake, and vmake are located out of alphabetic
*              order to satisfy the macintosh's limited range
*              for subroutine calls.  move built-in labels
*              beyond the block and pattern routines to get it
*              within 32k of the error routines.
*      c3.777  at scn46, allow colon, right paren and right
*              bracket to terminate = operator with default
*              null operand.
*      c3.778  added .ctet conditional for table entry trace.
*      c3.779  introduce cfp$l, the largest unsigned value
*              that may be stored in a one-word integer.  this
*              is done to accommodate machines where memory
*              addresses have the high-order address bit set.
*      c3.780  perform replace in place if first arg is buffer.
*      c3.781  perform reverse in place if first arg is buffer.
*      c3.782  change sysou to accept buffer as well as string
*              to be output.  change code at asg11 to prevent
*              conversion of buffer to string.
*      c3.783  optimize pos and rpos when it is the first node
*              of a pattern and has either an integer or simple
*              expression variable argument.  if unanchored mode
*              and the cursor is zero, it is advanced directly
*              to the desired cursor position.
*      c3.784  perform trim function in place if arg is buffer.
*      c3.785  add gtstb procedure to get a string or buffer
*              argument for replace, reverse, size, trim, etc.
*      c3.786  change leq, lgt, etc. to perform comparisons
*              without converting buffer arguments to strings.
*              this is done by changing lcomp to accept buffer
*              argument(s).  this also affects sort function,
*              which will compare two buffers as strings.
*      c3.787  change gtnum to use characters in buffer without
*              conversion to a string.  this implies that acomp
*              will perform arithmetic comparisons of buffers
*              without converting to strings first.
*      c3.788  perform comparisons of strings and buffers in
*              sortc.
*      c3.789  change insbf to allow insertion of a buffer into
*              a buffer without first converting it to a string.
*              note that this only works when the two buffers
*              are not the same.
*      c3.790  documentation change:  note that all of the block
*              move opcodes should have wa .gt. 0.  not all
*              implementations avoid moving objects when wa is
*              zero.
*      c3.791  change ident to provide buffer/buffer and
*              buffer/string comparisons, to accommodate users
*              who perform ident(buf) to check for null string
*              in buffer.
*      c3.792  added fullscan keyword initialized to one.  user
*              may set to any non-zero value, will receive an
*              error message if attempts to set to zero, since
*              quickscan mode is not supported.
*      c3.793  rewrote statement startup code at stmgo to only
*              perform checking of profiling, stcount tracing,
*              and statement counting if necessary.
*      c3.794  add additional exit to sysfc and ioput to signal
*              that i/o channel (fcblk) is already in use.
*              added error message numbers 289 and 290.
*      c3.795  added optional integer argument to date function
*              to specify format of date string returned by
*              sysdt.
*
*
*      version 3.6 to 3.6a (oct 83)
*      ---------------------------
*
*      changes
*      -------
*
*      c3.617  add .cnlf. if defined, then arguments to external
*              functions may be declared to have type file.
*              such arguments must have been used as second
*              arg to input() or output() and a pointer to the
*              fcb is passed to the external function.
*
*
*      version 3.5 to 3.6 (jun 83)
*      ---------------------------
*
*      codes used to identify authors are (sgd) for duff,
*      (reg) for  goldberg, and (lds) for shields.
*
*      bugs fixed
*      ----------
*      b3.601  (sgd) to fix multiple trap block problem in asign
*      b3.602  (sgd) patch in gtarr to fix null convert.
*      b3.603  (sgd) inserted missing wtb after sysmm calls.
*      b3.604  (sgd) use string length in hashs.
*      b3.605  (sgd) fixed serious parser problem
*              relating to (x y) on line being viewed as pattern
*              match.  fixed by addition of new cmtyp value
*              c$cnp (concatenation - not pattern match).
*      b3.606  (sgd) fixed exit(n) respecification code
*              to properly observe header semantics on return.
*      b3.607  (sgd) bypass prtpg call at initialization
*              following compilation if no output generated.
*              this prevents output files consisting of the
*              headers and a few blank lines when there is no
*              source listing and no compilation stats.
*              also fix timsx initialization in same code.
*      b3.608  (sgd) b$efc code did not check for
*              unconverted result returning null string.
*      b3.609  (sgd) load pfvbl field in retrn for
*              return tracing. this was causing bug on return
*              traces that tried to access the variable name.
*      b3.610  (sgd) fixed problem relating to compilation of
*              goto fields containing small integers
*              (in const sec).
*      b3.611  (reg) prevent clear() from clobbering protected
*              variables at label sclr5.
*      b3.612  (reg) fixed gtexp from accepting trailing
*              semicolon or colon. this is not a legal way
*              to end an expression.
*      b3.613  (reg) fixed difficulties with listings during
*              execution when no listing generated during
*              compilation. -list to code() caused bomb.
*              fix is to reset r$ttl and r$stl to nulls not 0
*              after compilation.
*              (listr and listt expect nulls)
*              when listing and statistics routed to different
*              file than execution output, error message is sent
*              to execution output (and gets separated from
*              ... in statement ... msg). labo1 calls sysax and
*              stopr does not call sysax if entered from labo1.
*      b3.614  (lds) fix misuse of wc just after asg10.
*      b3.615  (lds) add comment pointing out suspicious code
*              after tfn02
*      b3.616  (lds) fix inconsistent declaration of sorth.
*      b3.617  (lds) insert missing conditional tests on cnbf.
*      b3.618  (lds) fix some violations of minimal language
*              that had slipped past some translators.
*      b3.619  (lds) correct error introduced in fixing b3.614.
{{ejc{{{{{722
*      changes
*      -------
*
*
*      c3.601  (sgd) addition of .cnci and sysci (int to string
*              system routine option)
*      c3.602  (reg) changed iniln and and inils to 258
*      c3.603  (sgd) merged in profiler patches, repaired code.
*      c3.604  (sgd) added buffer type and symbol cnbf
*      c3.605  (sgd) added char function.  char(n) returns nth
*              character of host machine character set.
*      c3.606  (reg) added cfp$u to ease translation on smaller
*              systems - conditional .cucf
*      c3.607  (reg) added lower case support, conditional .culc
*      c3.608  (reg) added set i/o function, conditional .cust
*      c3.609  (reg) conditionalized page eject after call to
*              sysbx and added another before call to sysbx,
*              so that, if desired by the implementor,
*              standard output will reflect assignments made
*              by executing program only.
*              conditional .cuej controls - if defined then
*              eject is before call to sysbx.
*      c3.610  (lds) introduce .ctmd to support systm that
*              reports elapsed time in deciseconds instead of
*              milliseconds.
*      c3.611  (lds)  provide place for .def or .und for each
*              conditional  option, so that settings can be
*              changed without changing line numbers.
*              current settings are for 808x translation.
*      c3.612  (lds) obey (new) restriction that operand in
*              conditional branch instruction cannot have form
*              (x)+ in order to simplify translations for which
*              postincrement not readily available.
*      c3.613  (reg,lds) add op
*                    flc wreg
*              that folds character in wreg to upper case.
*              this op is used only if .culc is defined.
*              this change also involves addition of keyword
*              &case which when nonzero (the initial setting)
*              causes the case folding just described to be
*              done.
*      c3.614  (lds) add option .cs16 to permit initialization
*              of statement limit values to 32767 for 16 bit
*              machines.
*      c3.615  (lds) permit return point and entry point
*              addresses to be distinguished by their parity
*              instead of by lying within a certain range
*              of values.  introduce conditional symbols
*              .crpp  return points have odd parity
*              .cepp  entry points have odd parity
*      c3.616  (lds) introduce new minimal opcodes to branch
*              according to parity,
*                bev  opn,plbl  branch if address even
*                bod  opn,plbl  branch if address odd
*              an address is even if it is a multiple of cfp$b.
{{ejc{{{{{778
*      documentation revisions
*      -----------------------
*
*      d3.601  (lds) bring minimal machine description up to
*              date
*
{{ejc{{{{{785
*      version 3.4 to 3.5 (feb 79)
*      ---------------------------
*
*
*      bugs fixed
*      ----------
*
*      b3.401  prtst should be declared as an r type procedure.
*      b3.402  timing error if spitbol fails in dump.
*      b3.403  error in handling omitted args of operators.
*      b3.404  too many lines put on first page of listing.
*      b3.405  leading unary operator in eval erroneously needed
*              preceding blank.
*      b3.406  identifying name in dump of array or table values
*              was omitted.
*      b3.407  eval unable to return a deferred expression.
*      b3.408  illegal if setexit code branches to return.
*      b3.409  illegal on detaching input, output, terminal.
*
*      changes
*      -------
*
*      c3.401  -sequ and -nose control cards removed.
*      c3.402  option provided to suppress system identification
*              on listing.
*      c3.403  description of sysbx slightly revised.
*      c3.404  permissible to modify scblk length before taking
*              error returns from sysin, sysrd, sysri.
*      c3.405  conditional .cnld may be defined to omit load().
*      c3.406  conditional .cnex may be defined to omit exit().
*      c3.407  table now accepts a third argument specifying
*              default initial lookup value.
*      c3.408  routines sort, rsort for sorting arrays and table
*              introduced. specification is as in sitbol.
*              routines may be omitted by defining .cnsr .
*      c3.409  error in code(), eval() call now causes statement
*              failure but errtext keyword is still set.
*      c3.410  arg to code() may contain embedded control cards
*              and comment delimited by a semicolon.
*
*      documentation revisions
*      -----------------------
*
*      d3.401  purpose of restriction 2 in minimal section -6-
*              (operations on char values), erroneously stated
*              to be for cmc, rather than for ceq, cne.
*              descriptions of above opcodes revised.
*      d3.402  description of ent clarified.
*      d3.403  descriptions of several opcodes revised to remove
*              technically invalid literals e.g. =0 , *1.
*      d3.405  restricted use of letter z in minimal clarified.
*      d3.406  divide by zero explicitly mentioned in relation
*              to overflow setting.
{{ejc{{{{{839
*      version 3.3 to 3.4 (oct 78)
*      ---------------------------
*
*
*      bugs fixed
*      ----------
*
*      b3.301  illegal for erroneous eval() arg.
*      b3.302  address arithmetic overflow in alloc and alocs.
*      b3.303  -eject and -space ignored -nolist option.
*      b3.304  erroneous argument scan in load().
*      b3.305  erroneous plc on uninitialised r$cim in nexts.
*      b3.306  ldi used instead of mti after prv07.
*      b3.307  misuse of rmi at erra2.
*      b3.308  misuse of mti in hashs.
*      b3.309  bug in -sequ card sequence number checking.
*      b3.310  stack overflow error message not always printed.
*      b3.311  corrupt prototype print for traced arrays.
*      b3.312  pattern first arg in dupl caused error.
*      b3.313  omitted csc in s$rpd, erroneous csc in convert.
*      b3.314  misplaced btw in exbld.
*      b3.315  incorrect code in hashs.
*      b3.316  failure of load to scan integer arg.
*      b3.317  table access with negative integer arg. failed.
*      b3.318  error in returning result of loaded function.
*      b3.319  =e$srs used after ini01 instead of *e$srs.
*      b3.320  err used instead of erb after systu
*      b3.321  label could start with disallowed character.
*      b3.322  continue after setexit had bad heuristic.
{{ejc{{{{{869
*
*
*      changes
*      -------
*
*      c3.301  sysax and .csax introduced - see sysax
*              in procedures section.
*      c3.302  variable mxlen introduced. contains the maximum
*              size of a spitbol object and is not changeable
*              after initialisation. may be defaulted or set
*              explicitly by sysmx.
*      c3.303  syshs returns revised - see syshs.
*      c3.304  new minimal opcode aov to fix b3.302.
*      c3.305  inhibit stlimit check if stlimit made negative.
*      c3.306  cfp$m is required to be of form 2**n - 1.
*      c3.307  dupl made to conform to sil snobol4 standard.
*      c3.308  lch and sch actions more closely defined.
*      c3.309  batch initialisation code omitted if conditional
*              assembly symbol .cnbt (no batch) defined.
*      c3.310  (wa) contains argument count in sysex call.
*      c3.311  sysfc  may request allocation of static fcblk.
*      c3.312  if ia,wc overlap, restriction put on dumping/
*              restoring these registers.
*      c3.313  new listing option intermediate between compact
*              and extended provided (see syspp).
*      c3.314  revision of sysxi interface to permit options for
*              load module standard o/p file (see sysxi,syspp).
*      c3.315  last arg of substr may be omitted - treated
*              as remainder of string.
{{ejc{{{{{899
*      version 3.2 to 3.3 (jan 78)
*      ---------------------------
*
*      bugs fixed
*      ----------
*
*      b3.201  array reference and external function load
*              routines illegally accessed information
*              beyond the stack front.
*              similar fault in unanchored pattern matching.
*      b3.202  dump(1) produced dump(2) type output.
*      b3.203  wtb conversion omitted in code following
*              ini01, ini02, exbld.
*      b3.204  incorrect fail return from tfind in arref.
*      b3.205  endfile did not detach i/o associated variables.
*      b3.206  -space with omitted arg. failed
*      b3.207  looped if dump keyword non-zero after stack
*              overflow in garbage collect failure.
*      b3.208  failure in reading numbers with trailing blanks.
*
*      changes
*      -------
*
*      the extensive changes made here mostly result from a
*      snobol4 implementors meeting held at new york university
*      in august 1977. they are aimed at
*           (1) having spitbol conform to certain snobol4
*           language standards  and
*           (2) producing a stable definition of minimal by
*           carrying out a few essential revisions in the light
*           of experience in its use.
*
*      changes to spitbol
*      ------------------
*
*      c3.201  default values for keywords trim and anchor are
*              zero. on systems where records are customarily
*              handled without traling blanks, there is no
*              obligation to supply such blanks.
*      c3.202  default value of -inxx control card is -in72.
{{ejc{{{{{940
*      c3.203  the second argument of input and output is
*              permitted to be an integer as in snobol4.
*              in addition input(), output() now give a snobol4
*              statement failure if sysio uses the file not
*              found return.
*              the third argument has a recommended format and
*              to override its default delimiter (,) a
*              conditional assembly symbol, .ciod, is used.
*              interfaces to sysef,sysej,syfc,sysio,sysrw
*              are revised.
*              wc may now be used to return from sysio, a max
*              record length.
*      c3.204  a new configuration parameter cfp$f (scblk offset
*              is introduced. cfp$u is removed.
*      c3.205  implementation and version identification is
*              required - see sysid.
*      c3.206  routine sysmx returns the maximum length of
*              spitbol objects (strings arrays etc).  this
*              information is not now needed at time of entry to
*              spitbol and hence wc should be zero on entry.
*      c3.207  a conditional parameter .cnra permits assembly
*              of a more compact version with no real
*              arithmetic code.
*      c3.208  terminal is a new pre-associated variable
*              capable of performing input and output to an
*              online terminal.
*              sysri is a new routine used in the implementation
*              of this. see also syspp.
*      c3.209  the environment parameters e$--- are now
*              provided by the minimal translator using the
*              revised   equ  *   format (see c3.229 and start
*              of spitbol definitions section - some reordering
*              of symbols has occurred).
*      c3.210  the interface of sysxi has been slightly revised.
*              unavailability of i/o channels after exit(1),
*              exit(-1) is documented together with additional
*              error return usage for sysin,sysou,syspr,sysrd.
*      c3.211  spitbol error codes have been frozen - see c3.230
*      c3.212  the utility routines arref etc. are now
*              introduced by rtn statements.
*      c3.213  sysrl (record length for std input file) is
*              removed. since implementation of a general -inxxx
*              control card and an ability to specify max record
*              length using the third argument of input, sysrl
*              has become redundant.
*      c3.214  sysej and sysxi are now passed a chain linking
*              all fcblks in use.
*      c3.215  a special ending code in sysej is used when
*              attempts to use standard output channel fail.
*      c3.216  restriction c3.233 observed so simplifying
*              optimised translation of ent with omitted val.
{{ejc{{{{{992
*
*      changes to minimal
*      ------------------
*
*      c3.220  minimal opcodes dec, dim, inc, and bmp
*              are withdrawn and replaced by the more consistent
*              set dca, dcv, ica, icv.
*      c3.221  chs has been replaced by the more generally
*              useful zgb (still likely to be a no-op for most
*              implementations however).
*      c3.222  the set of character comparisons has been
*              reduced to ceq and cne to ease implementation
*              problems.
*      c3.223  opcode irz is removed and dvi, rmi orders are
*              redefined to conform to more common usage.
*      c3.224  new opcodes ssl and sss are defined. their use
*              permits return links for n type procedures to be
*              placed on a local stack if desired.
*      c3.225  opcode mnz complements zer. it moves a non-zero
*              flag to its destination.
*      c3.226  for some machines it is preferable for the stack
*              to build up rather than down. to permit this
*              without need for massive changes in minimal and
*              recoding of existing programs, a scheme has been
*              devised in which an additional register name, xt,
*              is used as a synonym for xl when this register
*              is involved in stack manipulation- see section 4.
*      c3.227  section 0 of a minimal program is renamed the
*              procedure section. it now contains, in addition
*              to exp, specifications of internal procedures
*              and routines by means of the inp and inr opcodes.
*      c3.228  the literal operand formats =int and *int have
*              been withdrawn. =dlbl and *dlbl must be used in
*              their stead.
*      c3.229  the format
*              label  equ  *nn
*              used to specify values supplied by the minimal
*              translator for char. codes etc. is replaced by
*              label  equ  *
*              where the order in which the definitions are
*              supplied by the translator should match the
*              order of occurrence in the definitions section.
*      c3.230  the format of err,erb opcodes is changed to
*              require a numeric operand.
*      c3.231  the rtn opcode is used to introduce routines
*              (which are quite distinct from procedures).
*      c3.232  conditional assembly directives may be nested.
*      c3.233  minor restriction placed on the omission of
*              val with the ent opcode.
{{ejc{{{{{1042
*      version 3.1 to 3.2 (aug 77)
*      ---------------------------
*
*      bugs fixed
*      ----------
*
*      b3.101  astonishing this was unnoticed for three years.
*              bad code for snobol4 integer divide, /, gave
*              wrong result for operands of opposite signs.
*              implementations have either wrongly translated
*              dvi and got correct result or correctly
*              translated dvi and got wrong result - leeds had
*              one of each. see also c3.106.
*              test program no. 1 now extended to check /
*              more thoroughly.
*      b3.102  garbage collection bug in scane
*
*      changes
*      -------
*
*      c3.101  option to use additional characters ch$ht,ch$vt
*              (horizontal and vertical tab) with same syntactic
*              significance as ch$bl (blank).
*      c3.102  option to use a set of shifted case alphabetic
*              characters ch$$a ... ch$$$.
*      c3.103  conditional assembly features are introduced into
*              minimal on account of the above.
*              see minimal documentation section for details
*              of above changes.
*      c3.104  lch and sch may use an x register first
*              operand as alternative to a w register.
*      c3.105  spitbol statement numbers in the listing may
*              optionally be padded to 6 or 8 chars instead of 5
*              by defining conditional assembly symbols
*              .csn6 or .csn8 .
*      c3.106  to fix bug 3.101. at moderate cost,
*              opcode irz (branch if integer divide remainder
*              zero) introduced.
*      c3.107  to handle possible machine dependency in string
*              hashing, chs (complete hashing of string) opcode
*              is introduced. probably a no-op on most machines
*              - not on the dec10.
*      c3.108  procedures patin,tfind,trace have been
*              modified to conform to the minimal standard
*              call and return regime.
*      c3.109  sysfc interface revised slightly to permit
*              osint to return a pointer to a privately
*              allocated fcblk which spitbol will return on
*              subsequent i/o - see sysfc doc.
*      c3.110  to remove inconsistencies in calling sequences,
*              all sys routines having access to a possible
*              fcblk have fcblk ptr or zero in reg. wa on entry.
*              change affects sysef, sysen, sysil, sysin,
*              sysou, sysrw.
*      c3.111  syspp bit allocated to provide
*               -noexec option on entry to spitbol.
{{ejc{{{{{1099
*
*      documentation revisions
*      -----------------------
*
*      d3.101  need to preserve registers in syspi, syspr,
*              sysrd calls was overstated.
{{ejc{{{{{1106
*      version 3.0 to 3.1 (mar 77)
*      ---------------------------
*
*      bugs fixed
*      ----------
*
*      b3.001  replace() could fail during pre-evaluation.
*              spitbol now signals an error for null or
*              unequally long 2nd and 3rd arguments.
*      b3.002  negative second arguments to dupl, lpad, rpad
*              caused spitbol to signal an error. now causes
*              return of null string or first arg respectively.
*      b3.003  brn-s used instead of ppm-s in s$sub.
*      b3.004  err used instead of erb after cmp30.
*      b3.005  b$pfc, s$cnv, s$def, arith and arref kept
*              information illegally above the stack top.
*      b3.006  pre-evaluation of constant parts of
*              complex gotos was erroneous.
*      b3.007  incorrect handling of labels compiled by code().
*      b3.008  the single use of trc (in s$rpl) was not in
*              accord with its definition. some translations of
*              trc may need revision now that the use
*              has been brought into line with definition.
*
*      changes
*      -------
*
*      a debate on a few weaknesses in minimal design has
*      been resolved by introducing 4 new opcodes.
*
*      c3.001  new minimal opcodes bmp and dim introduced
*              to augment inc and dec which are applicable
*              only to addresses.
*      c3.002  the opcode szc (store zero characters) had
*              a restricted applicability. it has been
*              replaced by the more general zer (zeroise).
*      c3.003  fcblks may be optionally allocated as xrblk-s or
*              xnblk-s  - see sysfc for vital information.
*      c3.004  control card processing has been recoded.
*              -inxxx allows specification of standard input
*              file record lengths other than 72 or 80, see also
*              sysrl. -sequ is ignored unless -in80 is in effect
*      c3.005  to enable efficient buffering of chars on
*              machines without char. handling orders, the
*              csc (complete store characters) instruction
*              is introduced. current implementations can
*              translate it as a no-op if it is of no benefit.
*      c3.006  integers 0,1,2 are treated specially.
*              icblks in static are used instead of
*              allocating space in dynamic.
{{ejc{{{{{1157
*
*      version 2.7 (june 76) to 3.0 (jan 77)
*      -------------------------------------
*
*      bugs fixed
*      ----------
*
*      b2.701  goes illegal if timed out during processing of
*              dump() call.
*      b2.702  goes illegal if spitbol error detected in args of
*              code() or eval(). bug fixed so that user now gets
*              a spitbol error report (trappable by setexit)
*              before statement failure.
*      b2.703  goes illegal in some circumstances when
*              multiple compilation errors occur in a statement
*      b2.704  goes illegal if garbage collector runs out of
*              stack space.
*      b2.705  control card processing incorrect for cdc 6400.
*      b2.706  incorrect handling of multiple occurrences of
*              chars in replace 2nd and 3rd args.
*      b2.707  stack overflow in pre-evaluation of replace in
*              cdc 6400 version.
*      b2.708  an explicit call of sysmw was coded in s$dat
*              instead of the mvw opcode.
*      b2.709  call of garbage collector whilst dumping
*              caused havoc.
*      b2.710  size restriction on spitbol objects (size must be
*              numerically less than lowest dynamic address)
*              was not enforced, with potential for catastrophe.
*      b2.711  deferred expressions involving alternation or
*              negation were incorrectly translated.
*      b2.712  listing of a compilation error at the end of a
*              long line could cause compiler to go illegal.
*      b2.713  incorrect -nofail code with success goto.
{{ejc{{{{{1192
*
*
*      changes
*      -------
*
*      (it is not anticipated that major revisions on this
*      scale will be frequent).
*
*      c2.701  default value of anchor keyword is set to 1. this
*              conflicts with snobol4 practice but is a
*              preferable default for most applications.
*      c2.702  if errtype is out of range the string in keyword
*              errtext is printed as the error message.
*      c2.703  if stlimit is exceeded, up to 10 more statements
*              may be obeyed to permit setexit trap to gain
*              control.
*      c2.704  the concept of an interactive channel is
*              introduced for implementations where an online
*              terminal may be used for spitbol. the standard
*              print file may be specified as interactive in
*              which case shorter title lines are output.
*              alternatively copies of compilation and
*              execution errors only may be sent to this channel
*      c2.705  printing of compilation statistics may be
*              suppressed.
*      c2.706  printing of execution statistics may be
*              suppressed.
*      c2.707  extended or compact listing format may be
*              selected.
*      c2.708  an initial -nolist option may be specified
*              before compilation starts.
*      c2.709  to specify choices implied by c2.704 to c2.708
*              syspp interface is revised and syspi is defined.
*      c2.710  compilation and execution time statistics
*              messages have been shortened.
*      c2.711  the exit function as in sitbol is introduced
*              to permit saving load modules - see sysxi, s$ext.
*      c2.712  diagnostic routines sysgb and sysgd have been
*              removed. they were useful in the early debugging
*              days but have fallen into disuse now.
*      c2.713  szc may have an operand of type opn instead of
*              type opw
*      c2.714  input/output association interface has been
*              revised. sysif,sysof have been consolidated into
*              the new system routine, sysio, and the
*              specification of sysfc has been slightly changed.
*      c2.715  configuration parameter mxlen has been withdrawn
*              and the maximum size of a spitbol object which
*              was formerly fixed at spitbol compile time by
*              reference to it may now be specified as a run
*              time option by placing a value in wc before entry
*              to spitbol. (see comment on dynamic area in
*              basic information section).
*      c2.716  a function, host, is introduced which yields
*              information about the host machine - see syshs
*              and s$hst.
{{ejc{{{{{1249
*
*      documentation revisions
*      -----------------------
*
*      d2.701  the description of mvc has been revised to
*              reflect the fact that some spitbol code sequences
*              rely on mvc not destroying wb. minor changes
*              have been made to mwb and mvw descriptions to
*              emphasise similarities in the implicit loops of
*              these orders.
*      d2.702  descriptions of dvi and rmi have been clarified.
*      d2.703  implementation of rsx,lsx,ceq,cge,cgt,chi,clo,clt
*              is optional at present since they are currently
*              unused. their use in later versions is not
*              excluded.
*      d2.704  impossibility of using stack for return links of
*              n type procedures is emphasised.
*      d2.705  notation (xl),(wc) etc in language description is
*              clarified.
*      d2.706  documentation of sysfc, sysio has been improved.
*      d2.707  opcode descriptions are cross referenced from
*              the alphabetical opcode list.
*      d2.708  general description of compiler has been moved to
*              the start of the compiler proper.
*      d2.709  definitions of environment parameters have been
*              put near the front of the definitions section.
{{ttl{27,minimal -- machine independent macro assembly lang.{{{{1276
{{ejc{{{{{1277
{{ttl{27,s p i t b o l  -- basic information{{{{1278
{{ejc{{{{{1279
*      general structure
*      -----------------
*      this program is a translator for a version of the snobol4
*      programming language. language details are contained in
*      the manual macro spitbol by dewar and mccann, technical
*      report 90, university of leeds 1976.
*      the implementation is discussed in dewar and mccann,
*      macro spitbol - a snobol4 compiler, software practice and
*      experience, 7, 95-113, 1977.
*      the language is as implemented by the btl translator
*      (griswold, poage and polonsky, prentice hall, 1971)
*      with the following principal exceptions.
*      1)   redefinition of standard system functions and
*           operators is not permitted.
*      2)   the value function is not provided.
*      3)   access tracing is provided in addition to the
*           other standard trace modes.
*      4)   the keyword stfcount is not provided.
*      5)   the keyword fullscan is not provided and all pattern
*           matching takes place in fullscan mode (i.e. with no
*           heuristics applied).
*      6)   a series of expressions separated by commas may
*           be grouped within parentheses to provide a selection
*           capability. the semantics are that the selection
*           assumes the value of the first expression within it
*           which succeeds as they are evaluated from the left.
*           if no expression succeeds the entire statement fails
*      7)   an explicit pattern matching operator is provided.
*           this is the binary query (see gimpel sigplan oct 74)
*      8)   the assignment operator is introduced as in the
*           gimpel reference.
*      9)   the exit function is provided for generating load
*           modules - cf. gimpels sitbol.
*      the method used in this program is to translate the
*      source code into an internal pseudo-code (see following
*      section). an interpretor is then used to execute this
*      generated pseudo-code. the nature of the snobol4 language
*      is such that the latter task is much more complex than
*      the actual translation phase. accordingly, nearly all the
*      code in the program section is concerned with the actual
*      execution of the snobol4 program.
{{ejc{{{{{1334
*      interpretive code format
*      ------------------------
*      the interpretive pseudo-code consists of a series of
*      address pointers. the exact format of the code is
*      described in connection with the cdblk format. the
*      purpose of this section is to give general insight into
*      the interpretive approach involved.
*      the basic form of the code is related to reverse polish.
*      in other words, the operands precede the operators which
*      are zero address operators. there are some exceptions to
*      these rules, notably the unary not operator and the
*      selection construction which clearly require advance
*      knowledge of the operator involved.
*      the operands are moved to the top of the main stack and
*      the operators are applied to the top stack entries. like
*      other versions of spitbol, this processor depends on
*      knowing whether operands are required by name or by value
*      and moves the appropriate object to the stack. thus no
*      name/value checks are included in the operator circuits.
*      the actual pointers in the code point to a block whose
*      first word is the address of the interpretor routine
*      to be executed for the code word.
*      in the case of operators, the pointer is to a word which
*      contains the address of the operator to be executed. in
*      the case of operands such as constants, the pointer is to
*      the operand itself. accordingly, all operands contain
*      a field which points to the routine to load the value of
*      the operand onto the stack. in the case of a variable,
*      there are three such pointers. one to load the value,
*      one to store the value and a third to jump to the label.
*      the handling of failure returns deserves special comment.
*      the location flptr contains the pointer to the location
*      on the main stack which contains the failure return
*      which is in the form of a byte offset in the current
*      code block (cdblk or exblk). when a failure occurs, the
*      stack is popped as indicated by the setting of flptr and
*      control is passed to the appropriate location in the
*      current code block with the stack pointer pointing to the
*      failure offset on the stack and flptr unchanged.
{{ejc{{{{{1381
*      internal data representations
*      -----------------------------
*      representation of values
*      a value is represented by a pointer to a block which
*      describes the type and particulars of the data value.
*      in general, a variable is a location containing such a
*      pointer (although in the case of trace associations this
*      is modified, see description of trblk).
*      the following is a list of possible datatypes showing the
*      type of block used to hold the value. the details of
*      each block format are given later.
*      datatype              block type
*      --------              ----------
*      array                 arblk or vcblk
*      code                  cdblk
*      expression            exblk or seblk
*      integer               icblk
*      name                  nmblk
*      pattern               p0blk or p1blk or p2blk
*      real                  rcblk
*      string                scblk
*      table                 tbblk
*      program datatype      pdblk
{{ejc{{{{{1420
*      representation of variables
*      ---------------------------
*      during the course of evaluating expressions, it is
*      necessary to generate names of variables (for example
*      on the left side of a binary equals operator). these are
*      not to be confused with objects of datatype name which
*      are in fact values.
*      from a logical point of view, such names could be simply
*      represented by a pointer to the appropriate value cell.
*      however in the case of arrays and program defined
*      datatypes, this would violate the rule that there must be
*      no pointers into the middle of a block in dynamic store.
*      accordingly, a name is always represented by a base and
*      offset. the base points to the start of the block
*      containing the variable value and the offset is the
*      offset within this block in bytes. thus the address
*      of the actual variable is determined by adding the base
*      and offset values.
*      the following are the instances of variables represented
*      in this manner.
*      1)   natural variable base is ptr to vrblk
*                            offset is *vrval
*      2)   table element    base is ptr to teblk
*                            offset is *teval
*      3)   array element    base is ptr to arblk
*                            offset is offset to element
*      4)   vector element   base is ptr to vcblk
*                            offset is offset to element
*      5)   prog def dtp     base is ptr to pdblk
*                            offset is offset to field value
*      in addition there are two cases of objects which are
*      like variables but cannot be handled in this manner.
*      these are called pseudo-variables and are represented
*      with a special base pointer as follows=
*      expression variable   ptr to evblk (see evblk)
*      keyword variable      ptr to kvblk (see kvblk)
*      pseudo-variables are handled as special cases by the
*      access procedure (acess) and the assignment procedure
*      (asign). see these two procedures for details.
{{ejc{{{{{1473
*      organization of data area
*      -------------------------
*      the data area is divided into two regions.
*      static area
*      the static area builds up from the bottom and contains
*      data areas which are allocated dynamically but are never
*      deleted or moved around. the macro-program itself
*      uses the static area for the following.
*      1)   all variable blocks (vrblk).
*      2)   the hash table for variable blocks.
*      3)   miscellaneous buffers and work areas (see program
*           initialization section).
*      in addition, the system procedures may use this area for
*      input/output buffers, external functions etc. space in
*      the static region is allocated by calling procedure alost
*      the following global variables define the current
*      location and size of the static area.
*      statb                 address of start of static area
*      state                 address+1 of last word in area.
*      the minimum size of static is given approximately by
*           12 + *e_hnb + *e_sts + space for alphabet string
*           and standard print buffer.
{{ejc{{{{{1507
*      dynamic area
*      the dynamic area is built upwards in memory after the
*      static region. data in this area must all be in standard
*      block formats so that it can be processed by the garbage
*      collector (procedure gbcol). gbcol compacts blocks down
*      in this region as required by space exhaustion and can
*      also move all blocks up to allow for expansion of the
*      static region.
*      with the exception of tables and arrays, no spitbol
*      object once built in dynamic memory is ever subsequently
*      modified. observing this rule necessitates a copying
*      action during string and pattern concatenation.
*      garbage collection is fundamental to the allocation of
*      space for values. spitbol uses a very efficient garbage
*      collector which insists that pointers into dynamic store
*      should be identifiable without use of bit tables,
*      marker bits etc. to satisfy this requirement, dynamic
*      memory must not start at too low an address and lengths
*      of arrays, tables, strings, code and expression blocks
*      may not exceed the numerical value of the lowest dynamic
*      address.
*      to avoid either penalizing users with modest
*      requirements or restricting those with greater needs on
*      host systems where dynamic memory is allocated in low
*      addresses, the minimum dynamic address may be specified
*      sufficiently high to permit arbitrarily large spitbol
*      objects to be created (with the possibility in extreme
*      cases of wasting large amounts of memory below the
*      start address). this minimum value is made available
*      in variable mxlen by a system routine, sysmx.
*      alternatively sysmx may indicate that a
*      default may be used in which dynamic is placed
*      at the lowest possible address following static.
*      the following global work cells define the location and
*      length of the dynamic area.
*      dnamb                 start of dynamic area
*      dnamp                 next available location
*      dname                 last available location + 1
*      dnamb is always higher than state since the alost
*      procedure maintains some expansion space above state.
*      *** dnamb must never be permitted to have a value less
*      than that in mxlen ***
*      space in the dynamic region is allocated by the alloc
*      procedure. the dynamic region may be used by system
*      procedures provided that all the rules are obeyed.
*      some of the rules are subtle so it is preferable for
*      osint to manage its own memory needs. spitbol procs
*      obey rules to ensure that no action can cause a garbage
*      collection except at such times as contents of xl, xr
*      and the stack are +clean+ (see comment before utility
*      procedures and in gbcol for more detail). note
*      that calls of alost may cause garbage collection (shift
*      of memory to free space). spitbol procs which call
*      system routines assume that they cannot precipitate
*      collection and this must be respected.
{{ejc{{{{{1570
*      register usage
*      --------------
*      (cp)                  code pointer register. used to
*                            hold a pointer to the current
*                            location in the interpretive pseudo
*                            code (i.e. ptr into a cdblk).
*      (xl,xr)               general index registers. usually
*                            used to hold pointers to blocks in
*                            dynamic storage. an important
*                            restriction is that the value in
*                            xl must be collectable for
*                            a garbage collect call. a value
*                            is collectable if it either points
*                            outside the dynamic area, or if it
*                            points to the start of a block in
*                            the dynamic area.
*      (xs)                  stack pointer. used to point to
*                            the stack front. the stack may
*                            build up or down and is used
*                            to stack subroutine return points
*                            and other recursively saved data.
*      (xt)                  an alternative name for xl during
*                            its use in accessing stacked items.
*      (wa,wb,wc)            general work registers. cannot be
*                            used for indexing, but may hold
*                            various types of data.
*      (ia)                  used for all signed integer
*                            arithmetic, both that used by the
*                            translator and that arising from
*                            use of snobol4 arithmetic operators
*      (ra)                  real accumulator. used for all
*                            floating point arithmetic.
{{ejc{{{{{1611
*      spitbol conditional assembly symbols
*      ------------------------------------
*      in the spitbol translator, the following conditional
*      assembly symbols are referred to. to incorporate the
*      features referred to, the minimal source should be
*      prefaced by suitable conditional assembly symbol
*      definitions.
*      in all cases it is permissible to default the definitions
*      in which case the additional features will be omitted
*      from the target code.
*      .caex                 define to allow up arrow for expon.
*      .caht                 define to include horizontal tab
*      .casl                 define to include 26 shifted lettrs
*      .cavt                 define to include vertical tab
*      .cbyt                 define for statistics in bytes
*      .ccmc                 define to include syscm function
*      .ccmk                 define to include compare keyword
*      .cepp                 define if entrys have odd parity
*      .cera                 define to include sysea function
*      .cexp                 define if spitbol pops sysex args
*      .cgbc                 define to include sysgc function
*      .cicc                 define to ignore bad control cards
*      .cinc                 define to add -include control card
*      .ciod                 define to not use default delimiter
*                              in processing 3rd arg of input()
*                              and output()
*      .cmth                 define to include math functions
*      .cnbf                 define to omit buffer extension
*      .cnbt                 define to omit batch initialisation
*      .cnci                 define to enable sysci routine
*      .cncr                 define to enable syscr routine
*      .cnex                 define to omit exit() code.
*      .cnld                 define to omit load() code.
*      .cnlf                 define to add file type for load()
*      .cnpf                 define to omit profile stuff
*      .cnra                 define to omit all real arithmetic
*      .cnsc                 define to no numeric-string compare
*      .cnsr                 define to omit sort, rsort
*      .cpol                 define if interface polling desired
*      .crel                 define to include reloc routines
*      .crpp                 define if returns have odd parity
*      .cs16                 define to initialize stlim to 32767
*      .cs32                 define to init stlim to 2147483647
*                            omit to take default of 50000
*      .csax                 define if sysax is to be called
*      .csed                 define to use sediment in gbcol
*      .csfn                 define to track source file names
*      .csln                 define if line number in code block
*      .csou                 define if output, terminal to sysou
*      .ctet                 define to table entry trace wanted
*      .ctmd                 define if systm unit is decisecond
*      .cucf                 define to include cfp_u
*      .cuej                 define to suppress needless ejects
*      .culk                 define to include &l/ucase keywords
*      .culc                 define to include &case (lc names)
*                            if cucl defined, must support
*                            minimal op flc wreg that folds
*                            argument to lower case
*      .cust                 define to include set() code
*                            conditional options
*                            since .undef not allowed if symbol
*                            not defined, a full comment line
*                            indicates symbol initially not
*                            defined.
*      .cbyt                 define for statistics in bytes
*      .ccmc                 define to include syscm function
*      .ccmk                 define to include compare keyword
*      .cepp                 define if entrys have odd parity
*      .cera                 define to include sysea function
*      .cexp                 define if spitbol pops sysex args
*      .cicc                 define to ignore bad control cards
*      .cinc                 define to add -include control card
*                            in processing 3rd arg of input()
*                            and output()
*      .cmth                 define to include math functions
*      .cnci                 define to enable sysci routine
*      .cncr                 define to enable syscr routine
*      .cnex                 define to omit exit() code.
*      .cnlf                 define to add file type to load()
*      .cnpf                 define to omit profile stuff
*      .cnra                 define to omit all real arithmetic
*      .cnsc                 define if no numeric-string compare
*      .cnsr                 define to omit sort, rsort
*      .cpol                 define if interface polling desired
*      .crel                 define to include reloc routines
*      .crpp                 define if returns have odd parity
*      .cs16                 define to initialize stlim to 32767
*      .cs32                 define to init stlim to 2147483647
*      .csed                 define to use sediment in gbcol
*      .csfn                 define to track source file names
*      .csln                 define if line number in code block
*      .csou                 define if output, terminal to sysou
*      .ctmd                 define if systm unit is decisecond
*      force definition of .ccmk if .ccmc is defined
{{ttl{27,s p i t b o l -- procedures section{{{{1732
*      this section starts with descriptions of the operating
*      system dependent procedures which are used by the spitbol
*      translator. all such procedures have five letter names
*      beginning with sys. they are listed in alphabetical
*      order.
*      all procedures have a  specification consisting of a
*      model call, preceded by a possibly empty list of register
*      contents giving parameters available to the procedure and
*      followed by a possibly empty list of register contents
*      required on return from the call or which may have had
*      their contents destroyed. only those registers explicitly
*      mentioned in the list after the call may have their
*      values changed.
*      the segment of code providing the external procedures is
*      conveniently referred to as osint (operating system
*      interface). the sysxx procedures it contains provide
*      facilities not usually available as primitives in
*      assembly languages. for particular target machines,
*      implementors may choose for some minimal opcodes which
*      do not have reasonably direct translations, to use calls
*      of additional procedures which they provide in osint.
*      e.g. mwb or trc might be translated as jsr sysmb,
*      jsr systc in some implementations.
*      in the descriptions, reference is made to --blk
*      formats (-- = a pair of letters). see the spitbol
*      definitions section for detailed descriptions of all
*      such block formats except fcblk for which sysfc should
*      be consulted.
*      section 0 contains inp,inr specifications of internal
*      procedures,routines. this gives a single pass translator
*      information making it easy to generate alternative calls
*      in the translation of jsr-s for procedures of different
*      types if this proves necessary.
{{sec{{{{start of procedures section{1770
{{ejc{{{{{1772
*      sysax -- after execution
{sysax{exp{1,0{{{define external entry point{1776
*      if the conditional assembly symbol .csax is defined,
*      this routine is called immediately after execution and
*      before printing of execution statistics or dump output.
*      purpose of call is for implementor to determine and
*      if the call is not required it will be omitted if .csax
*      is undefined. in this case sysax need not be coded.
*      jsr  sysax            call after execution
{{ejc{{{{{1788
*      sysbs -- backspace file
{sysbs{exp{1,3{{{define external entry point{1793
*      sysbs is used to implement the snobol4 function backspace
*      if the conditional assembly symbol .cbsp is defined.
*      the meaning is system dependent.  in general, backspace
*      repositions the file one record closer to the beginning
*      of file, such that a subsequent read or write will
*      operate on the previous record.
*      (wa)                  ptr to fcblk or zero
*      (xr)                  backspace argument (scblk ptr)
*      jsr  sysbs            call to backspace
*      ppm  loc              return here if file does not exist
*      ppm  loc              return here if backspace not allowed
*      ppm  loc              return here if i/o error
*      (wa,wb)               destroyed
*      the second error return is used for files for which
*      backspace is not permitted. for example, it may be expected
*      files on character devices are in this category.
{{ejc{{{{{1813
*      sysbx -- before execution
{sysbx{exp{1,0{{{define external entry point{1818
*      called after initial spitbol compilation and before
*      commencing execution in case osint needs
*      to assign files or perform other necessary services.
*      osint may also choose to send a message to online
*      terminal (if any) indicating that execution is starting.
*      jsr  sysbx            call before execution starts
{{ejc{{{{{1827
*      sysdc -- date check
{sysdc{exp{1,0{{{define external entry point{1921
*      sysdc is called to check that the expiry date for a trial
*      version of spitbol is unexpired.
*      jsr  sysdc            call to check date
*      return only if date is ok
{{ejc{{{{{1928
*      sysdm  -- dump core
{sysdm{exp{1,0{{{define external entry point{1932
*      sysdm is called by a spitbol program call of dump(n) with
*      n ge 4.  its purpose is to provide a core dump.
*      n could hold an encoding of the start adrs for dump and
*      amount to be dumped e.g.  n = 256*a + s , s = start adrs
*      in kilowords,  a = kilowords to dump
*      (xr)                  parameter n of call dump(n)
*      jsr  sysdm            call to enter routine
{{ejc{{{{{1942
*      sysdt -- get current date
{sysdt{exp{1,0{{{define external entry point{1946
*      sysdt is used to obtain the current date. the date is
*      returned as a character string in any format appropriate
*      to the operating system in use. it may also contain the
*      current time of day. sysdt is used to implement the
*      snobol4 function date().
*      (xr)                  parameter n of call date(n)
*      jsr  sysdt            call to get date
*      (xl)                  pointer to block containing date
*      the format of the block is like an scblk except that
*      the first word need not be set. the result is copied
*      into spitbol dynamic memory on return.
{{ejc{{{{{1962
*      sysea -- inform osint of compilation and runtime errors
{sysea{exp{1,1{{{define external entry point{1966
*      provides means for interface to take special actions on
*      errors
*      (wa)                  error code
*      (wb)                  line number
*      (wc)                  column number
*      (xr)                  system stage
*      (xl)                  file name (scblk)
*      jsr  sysea            call to sysea function
*      ppm  loc              suppress printing of error message
*      (xr)                  message to print (scblk) or 0
*      sysea may not return if interface chooses to retain
*      control.  closing files via the fcb chain will be the
*      responsibility of the interface.
*      all registers preserved
{{ejc{{{{{1988
*      sysef -- eject file
{sysef{exp{1,3{{{define external entry point{1992
*      sysef is used to write a page eject to a named file. it
*      may only be used for files where this concept makes
*      sense. note that sysef is not normally used for the
*      standard output file (see sysep).
*      (wa)                  ptr to fcblk or zero
*      (xr)                  eject argument (scblk ptr)
*      jsr  sysef            call to eject file
*      ppm  loc              return here if file does not exist
*      ppm  loc              return here if inappropriate file
*      ppm  loc              return here if i/o error
{{ejc{{{{{2005
*      sysej -- end of job
{sysej{exp{1,0{{{define external entry point{2009
*      sysej is called once at the end of execution to
*      terminate the run. the significance of the abend and
*      code values is system dependent. in general, the code
*      value should be made available for testing, and the
*      abend value should cause some post-mortem action such as
*      a dump. note that sysej does not return to its caller.
*      see sysxi for details of fcblk chain
*      (wa)                  value of abend keyword
*      (wb)                  value of code keyword
*      (xl)                  o or ptr to head of fcblk chain
*      jsr  sysej            call to end job
*      the following special values are used as codes in (wb)
*      999  execution suppressed
*      998  standard output file full or unavailable in a sysxi
*           load module. in these cases (wa) contains the number
*           of the statement causing premature termination.
{{ejc{{{{{2029
*      sysem -- get error message text
{sysem{exp{1,0{{{define external entry point{2033
*      sysem is used to obtain the text of err, erb calls in the
*      source program given the error code number. it is allowed
*      to return a null string if this facility is unavailable.
*      (wa)                  error code number
*      jsr  sysem            call to get text
*      (xr)                  text of message
*      the returned value is a pointer to a block in scblk
*      format except that the first word need not be set. the
*      string is copied into dynamic memory on return.
*      if the null string is returned either because sysem does
*      not provide error message texts or because wa is out of
*      range, spitbol will print the string stored in errtext
*      keyword.
{{ejc{{{{{2050
*      sysen -- endfile
{sysen{exp{1,3{{{define external entry point{2054
*      sysen is used to implement the snobol4 function endfile.
*      the meaning is system dependent. in general, endfile
*      implies that no further i/o operations will be performed,
*      but does not guarantee this to be the case. the file
*      should be closed after the call, a subsequent read
*      or write may reopen the file at the start or it may be
*      necessary to reopen the file via sysio.
*      (wa)                  ptr to fcblk or zero
*      (xr)                  endfile argument (scblk ptr)
*      jsr  sysen            call to endfile
*      ppm  loc              return here if file does not exist
*      ppm  loc              return here if endfile not allowed
*      ppm  loc              return here if i/o error
*      (wa,wb)               destroyed
*      the second error return is used for files for which
*      endfile is not permitted. for example, it may be expected
*      that the standard input and output files are in this
*      category.
{{ejc{{{{{2076
*      sysep -- eject printer page
{sysep{exp{1,0{{{define external entry point{2080
*      sysep is called to perform a page eject on the standard
*      printer output file (corresponding to syspr output).
*      jsr  sysep            call to eject printer output
{{ejc{{{{{2086
*      sysex -- call external function
{sysex{exp{1,3{{{define external entry point{2090
*      sysex is called to pass control to an external function
*      previously loaded with a call to sysld.
*      (xs)                  pointer to arguments on stack
*      (xl)                  pointer to control block (efblk)
*      (wa)                  number of arguments on stack
*      jsr  sysex            call to pass control to function
*      ppm  loc              return here if function call fails
*      ppm  loc              return here if insufficient memory
*      ppm  loc              return here if bad argument type
*      (xr)                  result returned
*      the arguments are stored on the stack with
*      the last argument at 0(xs). on return, xs
*      is popped past the arguments.
*      the form of the arguments as passed is that used in the
*      spitbol translator (see definitions and data structures
*      section). the control block format is also described
*      (under efblk) in this section.
*      there are two ways of returning a result.
*      1)   return a pointer to a block in dynamic storage. this
*           block must be in exactly correct format, including
*           the first word. only functions written with intimate
*           knowledge of the system will return in this way.
*      2)   string, integer and real results may be returned by
*           pointing to a pseudo-block outside dynamic memory.
*           this block is in icblk, rcblk or scblk format except
*           that the first word will be overwritten
*           by a type word on return and so need not
*           be correctly set. such a result is
*           copied into main storage before proceeding.
*           unconverted results may similarly be returned in a
*           pseudo-block which is in correct format including
*           type word recognisable by garbage collector since
*           block is copied into dynamic memory.
{{ejc{{{{{2135
*      sysfc -- file control block routine
{sysfc{exp{1,2{{{define external entry point{2139
*      see also sysio
*      input and output have 3 arguments referred to as shown
*           input(variable name,file arg1,file arg2)
*           output(variable name,file arg1,file arg2)
*      file arg1 may be an integer or string used to identify
*      an i/o channel. it is converted to a string for checking.
*      the exact significance of file arg2
*      is not rigorously prescribed but to improve portability,
*      the scheme described in the spitbol user manual
*      should be adopted when possible. the preferred form is
*      a string _f_,r_r_,c_c_,i_i_,...,z_z_  where
*      _f_ is an optional file name which is placed first.
*       remaining items may be omitted or included in any order.
*      _r_ is maximum record length
*      _c_ is a carriage control character or character string
*      _i_ is some form of channel identification used in the
*         absence of _f_ to associate the variable
*         with a file allocated dynamically by jcl commands at
*         spitbol load time.
*      ,...,z_z_ are additional fields.
*      if , (comma) cannot be used as a delimiter, .ciod
*      should be defined to introduce by conditional assembly
*      another delimiter (see
*        iodel  equ  *
*      early in definitions section).
*      sysfc is called when a variable is input or output
*      associated to check file arg1 and file arg2 and
*      to  report whether an fcblk (file control
*      block) is necessary and if so what size it should be.
*      this makes it possible for spitbol rather than osint to
*      allocate such a block in dynamic memory if required
*      or alternatively in static memory.
*      the significance of an fcblk , if one is requested, is
*      entirely up to the system interface. the only restriction
*      is that if the fcblk should appear to lie in dynamic
*      memory, pointers to it should be proper pointers to
*      the start of a recognisable and garbage collectable
*      block (this condition will be met if sysfc requests
*      spitbol to provide an fcblk).
*      an option is provided for osint to return a pointer in
*      xl to an fcblk which it privately allocated. this ptr
*      will be made available when i/o occurs later.
*      private fcblks may have arbitrary contents and spitbol
*      stores nothing in them.
{{ejc{{{{{2185
*      the requested size for an fcblk in dynamic memory
*      should allow a 2 word overhead for block type and
*      length fields. information subsequently stored in the
*      remaining words may be arbitrary if an xnblk (external
*      non-relocatable block) is requested. if the request is
*      for an xrblk (external relocatable block) the
*      contents of words should be collectable (i.e. any
*      apparent pointers into dynamic should be genuine block
*      pointers). these restrictions do not apply if an fcblk
*      is allocated outside dynamic or is not allocated at all.
*      if an fcblk is requested, its fields will be initialised
*      to zero before entry to sysio with the exception of
*      words 0 and 1 in which the block type and length
*      fields are placed for fcblks in dynamic memory only.
*      for the possible use of sysej and sysxi, if fcblks
*      are used, a chain is built so that they may all be
*      found - see sysxi for details.
*      if both file arg1 and file arg2 are null, calls of sysfc
*      and sysio are omitted.
*      if file arg1 is null (standard input/output file), sysfc
*      is called to check non-null file arg2 but any request
*      for an fcblk will be ignored, since spitbol handles the
*      standard files specially and cannot readily keep fcblk
*      pointers for them.
*      filearg1 is type checked by spitbol so further checking
*      may be unneccessary in many implementations.
*      file arg2 is passed so that sysfc may analyse and
*      check it. however to assist in this, spitbol also passes
*      on the stack the components of this argument with
*      file name, _f_ (otherwise null) extracted and stacked
*      first.
*      the other fields, if any, are extracted as substrings,
*      pointers to them are stacked and a count of all items
*      stacked is placed in wc. if an fcblk was earlier
*      allocated and pointed to via file arg1, sysfc is also
*      passed a pointer to this fcblk.
*      (xl)                  file arg1 scblk ptr (2nd arg)
*      (xr)                  filearg2 (3rd arg) or null
*      -(xs)...-(xs)         scblks for _f_,_r_,_c_,...
*      (wc)                  no. of stacked scblks above
*      (wa)                  existing file arg1 fcblk ptr or 0
*      (wb)                  0/3 for input/output assocn
*      jsr  sysfc            call to check need for fcblk
*      ppm  loc              invalid file argument
*      ppm  loc              fcblk already in use
*      (xs)                  popped (wc) times
*      (wa non zero)         byte size of requested fcblk
*      (wa=0,xl non zero)    private fcblk ptr in xl
*      (wa=xl=0)             no fcblk wanted, no private fcblk
*      (wc)                  0/1/2 request alloc of xrblk/xnblk
*                            /static block for use as fcblk
*      (wb)                  destroyed
{{ejc{{{{{2240
*      sysgc -- inform interface of garbage collections
{sysgc{exp{1,0{{{define external entry point{2244
*      provides means for interface to take special actions
*      prior to and after a garbage collection.
*      possible usages-
*      1. provide visible screen icon of garbage collection
*         in progress
*      2. inform virtual memory manager to ignore page access
*         patterns during garbage collection.  such accesses
*         typically destroy the page working set accumulated
*         by the program.
*      3. inform virtual memory manager that contents of memory
*         freed by garbage collection can be discarded.
*      (xr)                  non-zero if beginning gc
*                            =0 if completing gc
*      (wa)                  dnamb=start of dynamic area
*      (wb)                  dnamp=next available location
*      (wc)                  dname=last available location + 1
*      jsr  sysgc            call to sysgc function
*      all registers preserved
{{ejc{{{{{2268
*      syshs -- give access to host computer features
{syshs{exp{1,8{{{define external entry point{2272
*      provides means for implementing special features
*      on different host computers. the only defined entry is
*      that where all arguments are null in which case syshs
*      returns an scblk containing name of computer,
*      name of operating system and name of site separated by
*      colons. the scblk need not have a correct first field
*      as this is supplied on copying string to dynamic memory.
*      spitbol does no argument checking but does provide a
*      single error return for arguments checked as erroneous
*      by osint. it also provides a single execution error
*      return. if these are inadequate, use may be made of the
*      minimal error section direct as described in minimal
*      documentation, section 10.
*      several non-error returns are provided. the first
*      corresponds to the defined entry or, for implementation
*      defined entries, any string may be returned. the others
*      permit respectively,  return a null result, return with a
*      result to be stacked which is pointed at by xr, and a
*      return causing spitbol statement failure. if a returned
*      result is in dynamic memory it must obey garbage
*      collector rules. the only results copied on return
*      are strings returned via ppm loc3 return.
*      (wa)                  argument 1
*      (xl)                  argument 2
*      (xr)                  argument 3
*      (wb)                  argument 4
*      (wc)                  argument 5
*      jsr  syshs            call to get host information
*      ppm  loc1             erroneous arg
*      ppm  loc2             execution error
*      ppm  loc3             scblk ptr in xl or 0 if unavailable
*      ppm  loc4             return a null result
*      ppm  loc5             return result in xr
*      ppm  loc6             cause statement failure
*      ppm  loc7             return string at xl, length wa
*      ppm  loc8             return copy of result in xr
{{ejc{{{{{2311
*      sysid -- return system identification
{sysid{exp{1,0{{{define external entry point{2315
*      this routine should return strings to head the standard
*      printer output. the first string will be appended to
*      a heading line of the form
*           macro spitbol version v.v
*      supplied by spitbol itself. v.v are digits giving the
*      major version number and generally at least a minor
*      version number relating to osint should be supplied to
*      give say
*           macro spitbol version v.v(m.m)
*      the second string should identify at least the machine
*      and operating system.  preferably it should include
*      the date and time of the run.
*      optionally the strings may include site name of the
*      the implementor and/or machine on which run takes place,
*      unique site or copy number and other information as
*      appropriate without making it so long as to be a
*      nuisance to users.
*      the first words of the scblks pointed at need not be
*      correctly set.
*      jsr  sysid            call for system identification
*      (xr)                  scblk ptr for addition to header
*      (xl)                  scblk ptr for second header
{{ejc{{{{{2340
*      sysif -- switch to new include file
{sysif{exp{1,1{{{define external entry point{2345
*      sysif is used for include file processing, both to inform
*      the interface when a new include file is desired, and
*      when the end of file of an include file has been reached
*      and it is desired to return to reading from the previous
*      nested file.
*      it is the responsibility of sysif to remember the file
*      access path to the present input file before switching to
*      the new include file.
*      (xl)                  ptr to scblk or zero
*      (xr)                  ptr to vacant scblk of length cswin
*                            (xr not used if xl is zero)
*      jsr  sysif            call to change files
*      ppm  loc              unable to open file
*      (xr)                  scblk with full path name of file
*                            (xr not used if input xl is zero)
*      register xl points to an scblk containing the name of the
*      include file to which the interface should switch.  data
*      is fetched from the file upon the next call to sysrd.
*      sysif may have the ability to search multiple libraries
*      for the include file named in (xl).  it is therefore
*      required that the full path name of the file where the
*      file was finally located be returned in (xr).  it is this
*      name that is recorded along with the source statements,
*      and will accompany subsequent error messages.
*      register xl is zero to mark conclusion of use of an
*      include file.
{{ejc{{{{{2378
*      sysil -- get input record length
{sysil{exp{1,0{{{define external entry point{2383
*      sysil is used to get the length of the next input record
*      from a file previously input associated with a sysio
*      call. the length returned is used to establish a buffer
*      for a subsequent sysin call.  sysil also indicates to the
*      caller if this is a binary or text file.
*      (wa)                  ptr to fcblk or zero
*      jsr  sysil            call to get record length
*      (wa)                  length or zero if file closed
*      (wc)                  zero if binary, non-zero if text
*      no harm is done if the value returned is too long since
*      unused space will be reclaimed after the sysin call.
*      note that it is the sysil call (not the sysio call) which
*      causes the file to be opened as required for the first
*      record input from the file.
{{ejc{{{{{2402
*      sysin -- read input record
{sysin{exp{1,3{{{define external entry point{2406
*      sysin is used to read a record from the file which was
*      referenced in a prior call to sysil (i.e. these calls
*      always occur in pairs). the buffer provided is an
*      scblk for a string of length set from the sysil call.
*      if the actual length read is less than this, the length
*      field of the scblk must be modified before returning
*      unless buffer is right padded with zeroes.
*      it is also permissible to take any of the alternative
*      returns after scblk length has been modified.
*      (wa)                  ptr to fcblk or zero
*      (xr)                  pointer to buffer (scblk ptr)
*      jsr  sysin            call to read record
*      ppm  loc              endfile or no i/p file after sysxi
*      ppm  loc              return here if i/o error
*      ppm  loc              return here if record format error
*      (wa,wb,wc)            destroyed
{{ejc{{{{{2425
*      sysio -- input/output file association
{sysio{exp{1,2{{{define external entry point{2429
*      see also sysfc.
*      sysio is called in response to a snobol4 input or output
*      function call except when file arg1 and file arg2
*      are both null.
*      its call always follows immediately after a call
*      of sysfc. if sysfc requested allocation
*      of an fcblk, its address will be in wa.
*      for input files, non-zero values of _r_ should be
*      copied to wc for use in allocating input buffers. if _r_
*      is defaulted or not implemented, wc should be zeroised.
*      once a file has been opened, subsequent input(),output()
*      calls in which the second argument is identical with that
*      in a previous call, merely associate the additional
*      variable name (first argument) to the file and do not
*      result in re-opening the file.
*      in subsequent associated accesses to the file a pointer
*      to any fcblk allocated will be made available.
*      (xl)                  file arg1 scblk ptr (2nd arg)
*      (xr)                  file arg2 scblk ptr (3rd arg)
*      (wa)                  fcblk ptr (0 if none)
*      (wb)                  0 for input, 3 for output
*      jsr  sysio            call to associate file
*      ppm  loc              return here if file does not exist
*      ppm  loc              return if input/output not allowed
*      (xl)                  fcblk pointer (0 if none)
*      (wc)                  0 (for default) or max record lngth
*      (wa,wb)               destroyed
*      the second error return is used if the file named exists
*      but input/output from the file is not allowed. for
*      example, the standard output file may be in this category
*      as regards input association.
{{ejc{{{{{2464
*      sysld -- load external function
{sysld{exp{1,3{{{define external entry point{2468
*      sysld is called in response to the use of the snobol4
*      load function. the named function is loaded (whatever
*      this means), and a pointer is returned. the pointer will
*      be used on subsequent calls to the function (see sysex).
*      (xr)                  pointer to function name (scblk)
*      (xl)                  pointer to library name (scblk)
*      jsr  sysld            call to load function
*      ppm  loc              return here if func does not exist
*      ppm  loc              return here if i/o error
*      ppm  loc              return here if insufficient memory
*      (xr)                  pointer to loaded code
*      the significance of the pointer returned is up to the
*      system interface routine. the only restriction is that
*      if the pointer is within dynamic storage, it must be
*      a proper block pointer.
{{ejc{{{{{2487
*      sysmm -- get more memory
{sysmm{exp{1,0{{{define external entry point{2491
*      sysmm is called in an attempt to allocate more dynamic
*      memory. this memory must be allocated contiguously with
*      the current dynamic data area.
*      the amount allocated is up to the system to decide. any
*      value is acceptable including zero if allocation is
*      impossible.
*      jsr  sysmm            call to get more memory
*      (xr)                  number of additional words obtained
{{ejc{{{{{2503
*      sysmx -- supply mxlen
{sysmx{exp{1,0{{{define external entry point{2507
*      because of the method of garbage collection, no spitbol
*      object is allowed to occupy more bytes of memory than
*      the integer giving the lowest address of dynamic
*      (garbage collectable) memory. mxlen is the name used to
*      refer to this maximum length of an object and for most
*      users of most implementations, provided dynamic memory
*      starts at an address of at least a few thousand words,
*      there is no problem.
*      if the default starting address is less than say 10000 or
*      20000, then a load time option should be provided where a
*      user can request that he be able to create larger
*      objects. this routine informs spitbol of this request if
*      any. the value returned is either an integer
*      representing the desired value of mxlen (and hence the
*      minimum dynamic store address which may result in
*      non-use of some store) or zero if a default is acceptable
*      in which mxlen is set to the lowest address allocated
*      to dynamic store before compilation starts.
*      if a non-zero value is returned, this is used for keyword
*      maxlngth. otherwise the initial low address of dynamic
*      memory is used for this keyword.
*      jsr  sysmx            call to get mxlen
*      (wa)                  either mxlen or 0 for default
{{ejc{{{{{2533
*      sysou -- output record
{sysou{exp{1,2{{{define external entry point{2537
*      sysou is used to write a record to a file previously
*      associated with a sysio call.
*      (wa)                  ptr to fcblk
*                            or 0 for terminal or 1 for output
*      (xr)                  record to be written (scblk)
*      jsr  sysou            call to output record
*      ppm  loc              file full or no file after sysxi
*      ppm  loc              return here if i/o error
*      (wa,wb,wc)            destroyed
*      note that it is the sysou call (not the sysio call) which
*      causes the file to be opened as required for the first
*      record output to the file.
{{ejc{{{{{2559
*      syspi -- print on interactive channel
{syspi{exp{1,1{{{define external entry point{2563
*      if spitbol is run from an online terminal, osint can
*      request that messages such as copies of compilation
*      errors be sent to the terminal (see syspp). if relevant
*      reply was made by syspp then syspi is called to send such
*      messages to the interactive channel.
*      syspi is also used for sending output to the terminal
*      through the special variable name, terminal.
*      (xr)                  ptr to line buffer (scblk)
*      (wa)                  line length
*      jsr  syspi            call to print line
*      ppm  loc              failure return
*      (wa,wb)               destroyed
{{ejc{{{{{2579
*      syspl -- provide interactive control of spitbol
{syspl{exp{1,3{{{define external entry point{2583
*      provides means for interface to take special actions,
*      such as interrupting execution, breakpointing, stepping,
*      and expression evaluation.  these last three options are
*      not presently implemented by the code calling syspl.
*      (wa)                  opcode as follows-
*                            =0 poll to allow osint to interrupt
*                            =1 breakpoint hit
*                            =2 completion of statement stepping
*                            =3 expression evaluation result
*      (wb)                  statement number
*      r_fcb                 o or ptr to head of fcblk chain
*      jsr  syspl            call to syspl function
*      ppm  loc              user interruption
*      ppm  loc              step one statement
*      ppm  loc              evaluate expression
*      ---                   resume execution
*                            (wa) = new polling interval
{{ejc{{{{{2606
*      syspp -- obtain print parameters
{syspp{exp{1,0{{{define external entry point{2610
*      syspp is called once during compilation to obtain
*      parameters required for correct printed output format
*      and to select other options. it may also be called again
*      after sysxi when a load module is resumed. in this
*      case the value returned in wa may be less than or equal
*      to that returned in initial call but may not be
*      greater.
*      the information returned is -
*      1.   line length in chars for standard print file
*      2.   no of lines/page. 0 is preferable for a non-paged
*           device (e.g. online terminal) in which case listing
*           page throws are suppressed and page headers
*           resulting from -title,-stitl lines are kept short.
*      3.   an initial -nolist option to suppress listing unless
*           the program contains an explicit -list.
*      4.   options to suppress listing of compilation and/or
*           execution stats (useful for established programs) -
*           combined with 3. gives possibility of listing
*           file never being opened.
*      5.   option to have copies of errors sent to an
*           interactive channel in addition to standard printer.
*      6.   option to keep page headers short (e.g. if listing
*           to an online terminal).
*      7.   an option to choose extended or compact listing
*           format. in the former a page eject and in the latter
*           a few line feeds precede the printing of each
*           of-- listing, compilation statistics, execution
*           output and execution statistics.
*      8.   an option to suppress execution as though a
*           -noexecute card were supplied.
*      9.   an option to request that name /terminal/  be pre-
*           associated to an online terminal via syspi and sysri
*      10.  an intermediate (standard) listing option requiring
*           that page ejects occur in source listings. redundant
*           if extended option chosen but partially extends
*           compact option.
*      11.  option to suppress sysid identification.
*      jsr  syspp            call to get print parameters
*      (wa)                  print line length in chars
*      (wb)                  number of lines/page
*      (wc)                  bits value ...mlkjihgfedcba where
*                            a = 1 to send error copy to int.ch.
*                            b = 1 means std printer is int. ch.
*                            c = 1 for -nolist option
*                            d = 1 to suppress compiln. stats
*                            e = 1 to suppress execn. stats
*                            f = 1/0 for extnded/compact listing
*                            g = 1 for -noexecute
*                            h = 1 pre-associate /terminal/
*                            i = 1 for standard listing option.
*                            j = 1 suppresses listing header
*                            k = 1 for -print
*                            l = 1 for -noerrors
{{ejc{{{{{2672
*      syspr -- print line on standard output file
{syspr{exp{1,1{{{define external entry point{2676
*      syspr is used to print a single line on the standard
*      output file.
*      (xr)                  pointer to line buffer (scblk)
*      (wa)                  line length
*      jsr  syspr            call to print line
*      ppm  loc              too much o/p or no file after sysxi
*      (wa,wb)               destroyed
*      the buffer pointed to is the length obtained from the
*      syspp call and is filled out with trailing blanks. the
*      value in wa is the actual line length which may be less
*      than the maximum line length possible. there is no space
*      control associated with the line, all lines are printed
*      single spaced. note that null lines (wa=0) are possible
*      in which case a blank line is to be printed.
*      the error exit is used for systems which limit the amount
*      of printed output. if possible, printing should be
*      permitted after this condition has been signalled once to
*      allow for dump and other diagnostic information.
*      assuming this to be possible, spitbol may make more syspr
*      calls. if the error return occurs another time, execution
*      is terminated by a call of sysej with ending code 998.
{{ejc{{{{{2702
*      sysrd -- read record from standard input file
{sysrd{exp{1,1{{{define external entry point{2706
*      sysrd is used to read a record from the standard input
*      file. the buffer provided is an scblk for a string the
*      length of which in characters is given in wc, this
*      corresponding to the maximum length of string which
*      spitbol is prepared to receive. at compile time it
*      corresponds to xxx in the most recent -inxxx card
*      (default 72) and at execution time to the most recent
*      ,r_r_ (record length) in the third arg of an input()
*      statement for the standard input file (default 80).
*      if fewer than (wc) characters are read, the length
*      field of the scblk must be adjusted before returning
*      unless the buffer is right padded with zeroes.
*      it is also permissible to take the alternative return
*      after such an adjustment has been made.
*      spitbol may continue to make calls after an endfile
*      return so this routine should be prepared to make
*      repeated endfile returns.
*      (xr)                  pointer to buffer (scblk ptr)
*      (wc)                  length of buffer in characters
*      jsr  sysrd            call to read line
*      ppm  loc              endfile or no i/p file after sysxi
*                            or input file name change.  if
*                            the former, scblk length is zero.
*                            if input file name change, length
*                            is non-zero. caller should re-issue
*                            sysrd to obtain input record.
*      (wa,wb,wc)            destroyed
{{ejc{{{{{2738
*      sysri -- read record from interactive channel
{sysri{exp{1,1{{{define external entry point{2742
*      reads a record from online terminal for spitbol variable,
*      terminal. if online terminal is unavailable then code the
*      endfile return only.
*      the buffer provided is of length 258 characters. sysri
*      should replace the count in the second word of the scblk
*      by the actual character count unless buffer is right
*      padded with zeroes.
*      it is also permissible to take the alternative
*      return after adjusting the count.
*      the end of file return may be used if this makes
*      sense on the target machine (e.g. if there is an
*      eof character.)
*      (xr)                  ptr to 258 char buffer (scblk ptr)
*      jsr  sysri            call to read line from terminal
*      ppm  loc              end of file return
*      (wa,wb,wc)            may be destroyed
{{ejc{{{{{2761
*      sysrw -- rewind file
{sysrw{exp{1,3{{{define external entry point{2765
*      sysrw is used to rewind a file i.e. reposition the file
*      at the start before the first record. the file should be
*      closed and the next read or write call will open the
*      file at the start.
*      (wa)                  ptr to fcblk or zero
*      (xr)                  rewind arg (scblk ptr)
*      jsr  sysrw            call to rewind file
*      ppm  loc              return here if file does not exist
*      ppm  loc              return here if rewind not allowed
*      ppm  loc              return here if i/o error
{{ejc{{{{{2778
*      systm -- get execution time so far
{systm{exp{1,0{{{define external entry point{2805
*      systm is used to obtain the amount of execution time
*      used so far since spitbol was given control. the units
*      are described as microseconds in the spitbol output, but
*      the exact meaning is system dependent. where appropriate,
*      this value should relate to processor rather than clock
*      timing values.
*      if the symbol .ctmd is defined, the units are described
*      as deciseconds (0.1 second).
*      jsr  systm            call to get timer value
*      (ia)                  time so far in micliseconds
*                            (deciseconds if .ctmd defined)
{{ejc{{{{{2819
*      systt -- trace toggle
{systt{exp{1,0{{{define external entry point{2823
*      called by spitbol function trace() with no args to
*      toggle the system trace switch.  this permits tracing of
*      labels in spitbol code to be turned on or off.
*      jsr  systt            call to toggle trace switch
{{ejc{{{{{2830
*      sysul -- unload external function
{sysul{exp{1,0{{{define external entry point{2834
*      sysul is used to unload a function previously
*      loaded with a call to sysld.
*      (xr)                  ptr to control block (efblk)
*      jsr  sysul            call to unload function
*      the function cannot be called following a sysul call
*      until another sysld call is made for the same function.
*      the efblk contains the function code pointer and also a
*      pointer to the vrblk containing the function name (see
*      definitions and data structures section).
{{ejc{{{{{2850
*      sysxi -- exit to produce load module
{sysxi{exp{1,2{{{define external entry point{2854
*      when sysxi is called, xl contains either a string pointer
*      or zero. in the former case, the string gives the
*      character name of a program. the intention is that
*      spitbol execution should be terminated forthwith and
*      the named program loaded and executed. this type of chain
*      execution is very system dependent and implementors may
*      choose to omit it or find it impossible to provide.
*      if (xl) is zero,ia contains one of the following integers
*      -1, -2, -3, -4
*           create if possible a load module containing only the
*           impure area of memory which needs to be loaded with
*           a compatible pure segment for subsequent executions.
*           version numbers to check compatibility should be
*           kept in both segments and checked on loading.
*           to assist with this check, (xr) on entry is a
*           pointer to an scblk containing the spitbol major
*           version number v.v (see sysid).  the file thus
*           created is called a save file.
*      0    if possible, return control to job control
*           command level. the effect if available will be
*           system dependent.
*      +1, +2, +3, +4
*           create if possible a load module from all of
*           memory. it should be possible to load and execute
*           this module directly.
*      in the case of saved load modules, the status of open
*      files is not preserved and implementors may choose to
*      offer means of attaching files before execution of load
*      modules starts or leave it to the user to include
*      suitable input(), output() calls in his program.
*      sysxi should make a note that no i/o channels,
*      including standard files, have files attached so that
*      calls of sysin, sysou, syspr, sysrd should fail unless
*      new associations are made for the load module.
*      at least in the case of the standard output file, it is
*      recommended that either the user be required to attach
*      a file or that a default file is attached, since the
*      problem of error messages generated by the load module
*      is otherwise severe. as a last resort, if spitbol
*      attempts to write to the standard output file and gets a
*      reply indicating that such ouput is unacceptable it stops
*      by using an entry to sysej with ending code 998.
*      as described below, passing of some arguments makes it
*      clear that load module will use a standard output file.
*      if use is made of fcblks for i/o association, spitbol
*      builds a chain so that those in use may be found in sysxi
*      and sysej. the nodes are 4 words long. third word
*      contains link to next node or 0, fourth word contains
*      fcblk pointer.
{{ejc{{{{{2910
*      sysxi (continued)
*      (xl)                  zero or scblk ptr to first argument
*      (xr)                  ptr to v.v scblk
*      (ia)                  signed integer argument
*      (wa)                  scblk ptr to second argument
*      (wb)                  0 or ptr to head of fcblk chain
*      jsr  sysxi            call to exit
*      ppm  loc              requested action not possible
*      ppm  loc              action caused irrecoverable error
*      (wb,wc,ia,xr,xl,cp)   should be preserved over call
*      (wa)                  0 in all cases except sucessful
*                            performance of exit(4) or exit(-4),
*                            in which case 1 should be returned.
*      loading and running the load module or returning from
*      jcl command level causes execution to resume at the point
*      after the error returns which follow the call of sysxi.
*      the value passed as exit argument is used to indicate
*      options required on resumption of load module.
*      +1 or -1 require that on resumption, sysid and syspp be
*      called and a heading printed on the standard output file.
*      +2 or -2 indicate that syspp will be called but not sysid
*      and no heading will be put on standard output file.
*      above options have the obvious implication that a
*      standard o/p file must be provided for the load module.
*      +3, +4, -3 or -4 indicate calls of neither sysid nor
*      syspp and no heading will be placed on standard output
*      file.
*      +4 or -4 indicate that execution is to continue after
*      creation of the save file or load module, although all
*      files will be closed by the sysxi action.  this permits
*      the user to checkpoint long-running programs while
*      continuing execution.
*      no return from sysxi is possible if another program
*      is loaded and entered.
{{ejc{{{{{2950
*      introduce the internal procedures.
{acess{inp{25,r{1,1{{{2954
{acomp{inp{25,n{1,5{{{2955
{alloc{inp{25,e{1,0{{{2956
{alocs{inp{25,e{1,0{{{2961
{alost{inp{25,e{1,0{{{2962
{arith{inp{25,n{1,3{{{2970
{asign{inp{25,r{1,1{{{2972
{asinp{inp{25,r{1,1{{{2973
{blkln{inp{25,e{1,0{{{2974
{cdgcg{inp{25,e{1,0{{{2975
{cdgex{inp{25,r{1,0{{{2976
{cdgnm{inp{25,r{1,0{{{2977
{cdgvl{inp{25,r{1,0{{{2978
{cdwrd{inp{25,e{1,0{{{2979
{cmgen{inp{25,r{1,0{{{2980
{cmpil{inp{25,e{1,0{{{2981
{cncrd{inp{25,e{1,0{{{2982
{copyb{inp{25,n{1,1{{{2983
{dffnc{inp{25,e{1,0{{{2984
{dtach{inp{25,e{1,0{{{2985
{dtype{inp{25,e{1,0{{{2986
{dumpr{inp{25,e{1,0{{{2987
{ermsg{inp{25,e{1,0{{{2992
{ertex{inp{25,e{1,0{{{2993
{evali{inp{25,r{1,4{{{2994
{evalp{inp{25,r{1,1{{{2995
{evals{inp{25,r{1,3{{{2996
{evalx{inp{25,r{1,1{{{2997
{exbld{inp{25,e{1,0{{{2998
{expan{inp{25,e{1,0{{{2999
{expap{inp{25,e{1,1{{{3000
{expdm{inp{25,n{1,0{{{3001
{expop{inp{25,n{1,0{{{3002
{filnm{inp{25,e{1,0{{{3004
{gbcol{inp{25,e{1,0{{{3009
{gbcpf{inp{25,e{1,0{{{3010
{gtarr{inp{25,e{1,2{{{3011
{{ejc{{{{{3012
{gtcod{inp{25,e{1,1{{{3013
{gtexp{inp{25,e{1,1{{{3014
{gtint{inp{25,e{1,1{{{3015
{gtnum{inp{25,e{1,1{{{3016
{gtnvr{inp{25,e{1,1{{{3017
{gtpat{inp{25,e{1,1{{{3018
{gtrea{inp{25,e{1,1{{{3021
{gtsmi{inp{25,n{1,2{{{3023
{gtstg{inp{25,n{1,1{{{3028
{gtvar{inp{25,e{1,1{{{3029
{hashs{inp{25,e{1,0{{{3030
{icbld{inp{25,e{1,0{{{3031
{ident{inp{25,e{1,1{{{3032
{inout{inp{25,e{1,0{{{3033
{insta{inp{25,e{1,0{{{3038
{iofcb{inp{25,n{1,3{{{3039
{ioppf{inp{25,n{1,0{{{3040
{ioput{inp{25,n{1,7{{{3041
{ktrex{inp{25,r{1,0{{{3042
{kwnam{inp{25,n{1,0{{{3043
{lcomp{inp{25,n{1,5{{{3044
{listr{inp{25,e{1,0{{{3045
{listt{inp{25,e{1,0{{{3046
{newfn{inp{25,e{1,0{{{3048
{nexts{inp{25,e{1,0{{{3050
{patin{inp{25,n{1,2{{{3051
{patst{inp{25,n{1,1{{{3052
{pbild{inp{25,e{1,0{{{3053
{pconc{inp{25,e{1,0{{{3054
{pcopy{inp{25,n{1,0{{{3055
{prflr{inp{25,e{1,0{{{3058
{prflu{inp{25,e{1,0{{{3059
{prpar{inp{25,e{1,0{{{3061
{prtch{inp{25,e{1,0{{{3062
{prtic{inp{25,e{1,0{{{3063
{prtis{inp{25,e{1,0{{{3064
{prtin{inp{25,e{1,0{{{3065
{prtmi{inp{25,e{1,0{{{3066
{prtmm{inp{25,e{1,0{{{3067
{prtmx{inp{25,e{1,0{{{3068
{prtnl{inp{25,r{1,0{{{3069
{prtnm{inp{25,r{1,0{{{3070
{prtnv{inp{25,e{1,0{{{3071
{prtpg{inp{25,e{1,0{{{3072
{prtps{inp{25,e{1,0{{{3073
{prtsn{inp{25,e{1,0{{{3074
{prtst{inp{25,r{1,0{{{3075
{{ejc{{{{{3076
{prttr{inp{25,e{1,0{{{3077
{prtvl{inp{25,r{1,0{{{3078
{prtvn{inp{25,e{1,0{{{3079
{rcbld{inp{25,e{1,0{{{3082
{readr{inp{25,e{1,0{{{3084
{relaj{inp{25,e{1,0{{{3086
{relcr{inp{25,e{1,0{{{3087
{reldn{inp{25,e{1,0{{{3088
{reloc{inp{25,e{1,0{{{3089
{relst{inp{25,e{1,0{{{3090
{relws{inp{25,e{1,0{{{3091
{rstrt{inp{25,e{1,0{{{3093
{sbstr{inp{25,e{1,0{{{3097
{scane{inp{25,e{1,0{{{3098
{scngf{inp{25,e{1,0{{{3099
{setvr{inp{25,e{1,0{{{3100
{sorta{inp{25,n{1,1{{{3103
{sortc{inp{25,e{1,1{{{3104
{sortf{inp{25,e{1,0{{{3105
{sorth{inp{25,n{1,0{{{3106
{start{inp{25,e{1,0{{{3108
{stgcc{inp{25,e{1,0{{{3109
{tfind{inp{25,e{1,1{{{3110
{tmake{inp{25,e{1,0{{{3111
{trace{inp{25,n{1,2{{{3112
{trbld{inp{25,e{1,0{{{3113
{trimr{inp{25,e{1,0{{{3114
{trxeq{inp{25,r{1,0{{{3115
{vmake{inp{25,e{1,1{{{3116
{xscan{inp{25,e{1,0{{{3117
{xscni{inp{25,n{1,2{{{3118
*      introduce the internal routines
{arref{inr{{{{{3122
{cfunc{inr{{{{{3123
{exfal{inr{{{{{3124
{exint{inr{{{{{3125
{exits{inr{{{{{3126
{exixr{inr{{{{{3127
{exnam{inr{{{{{3128
{exnul{inr{{{{{3129
{exrea{inr{{{{{3132
{exsid{inr{{{{{3134
{exvnm{inr{{{{{3135
{failp{inr{{{{{3136
{flpop{inr{{{{{3137
{indir{inr{{{{{3138
{match{inr{{{{{3139
{retrn{inr{{{{{3140
{stcov{inr{{{{{3141
{stmgo{inr{{{{{3142
{stopr{inr{{{{{3143
{succp{inr{{{{{3144
{sysab{inr{{{{{3145
{systu{inr{{{{{3146
{{ttl{27,s p i t b o l -- definitions and data structures{{{{3147
*      this section contains all symbol definitions and also
*      pictures of all data structures used in the system.
{{sec{{{{start of definitions section{3151
*      definitions of machine parameters
*      the minimal translator should supply appropriate values
*      for the particular target machine for all the
*      equ  *
*      definitions given at the start of this section.
*      note that even if conditional assembly is used to omit
*      some feature (e.g. real arithmetic) a full set of cfp_-
*      values must be supplied. use dummy values if genuine
*      ones are not needed.
{cfp_a{equ{24,256{{{number of characters in alphabet{3164
{cfp_b{equ{24,8{{{bytes/word addressing factor{3166
{cfp_c{equ{24,8{{{number of characters per word{3168
{cfp_f{equ{24,16{{{offset in bytes to chars in{3170
*                            scblk. see scblk format.
{cfp_i{equ{24,1{{{number of words in integer constant{3173
{cfp_m{equ{24,9223372036854775807{{{max positive integer in one word{3175
{cfp_n{equ{24,64{{{number of bits in one word{3177
*      the following definitions require the supply of either
*      a single parameter if real arithmetic is omitted or
*      three parameters if real arithmetic is included.
{cfp_r{equ{24,1{{{number of words in real constant{3187
{cfp_s{equ{24,9{{{number of sig digs for real output{3189
{cfp_x{equ{24,3{{{max digits in real exponent{3191
{mxdgs{equ{24,cfp_s+cfp_x{{{max digits in real number{3202
*      max space for real (for +0.e+) needs five more places
{nstmx{equ{24,mxdgs+5{{{max space for real{3207
*      the following definition for cfp_u supplies a realistic
*      upper bound on the size of the alphabet.  cfp_u is used
*      to save space in the scane bsw-iff-esw table and to ease
*      translation storage requirements.
{cfp_u{equ{24,128{{{realistic upper bound on alphabet{3217
{{ejc{{{{{3219
*      environment parameters
*      the spitbol program is essentially independent of
*      the definitions of these parameters. however, the
*      efficiency of the system may be affected. consequently,
*      these parameters may require tuning for a given version
*      the values given in comments have been successfully used.
*      e_srs is the number of words to reserve at the end of
*      storage for end of run processing. it should be
*      set as small as possible without causing memory overflow
*      in critical situations (e.g. memory overflow termination)
*      and should thus reserve sufficient space at least for
*      an scblk containing say 30 characters.
{e_srs{equ{24,100{{{30 words{3236
*      e_sts is the number of words grabbed in a chunk when
*      storage is allocated in the static region. the minimum
*      permitted value is 256/cfp_b. larger values will lead
*      to increased efficiency at the cost of wasting memory.
{e_sts{equ{24,1000{{{500 words{3243
*      e_cbs is the size of code block allocated initially and
*      the expansion increment if overflow occurs. if this value
*      is too small or too large, excessive garbage collections
*      will occur during compilation and memory may be lost
*      in the case of a too large value.
{e_cbs{equ{24,500{{{500 words{3251
*      e_hnb is the number of bucket headers in the variable
*      hash table. it should always be odd. larger values will
*      speed up compilation and indirect references at the
*      expense of additional storage for the hash table itself.
{e_hnb{equ{24,257{{{127 bucket headers{3258
*      e_hnw is the maximum number of words of a string
*      name which participate in the string hash algorithm.
*      larger values give a better hash at the expense of taking
*      longer to compute the hash. there is some optimal value.
{e_hnw{equ{24,3{{{6 words{3265
*      e_fsp.  if the amount of free space left after a garbage
*      collection is small compared to the total amount of space
*      in use garbage collector thrashing is likely to occur as
*      this space is used up.  e_fsp is a measure of the
*      minimum percentage of dynamic memory left as free space
*      before the system routine sysmm is called to try to
*      obtain more memory.
{e_fsp{equ{24,15{{{15 percent{3275
*      e_sed.  if the amount of free space left in the sediment
*      after a garbage collection is a significant fraction of
*      the new sediment size, the sediment is marked for
*      collection on the next call to the garbage collector.
{e_sed{equ{24,25{{{25 percent{3283
{{ejc{{{{{3285
*      definitions of codes for letters
{ch_la{equ{24,97{{{letter a{3289
{ch_lb{equ{24,98{{{letter b{3290
{ch_lc{equ{24,99{{{letter c{3291
{ch_ld{equ{24,100{{{letter d{3292
{ch_le{equ{24,101{{{letter e{3293
{ch_lf{equ{24,102{{{letter f{3294
{ch_lg{equ{24,103{{{letter g{3295
{ch_lh{equ{24,104{{{letter h{3296
{ch_li{equ{24,105{{{letter i{3297
{ch_lj{equ{24,106{{{letter j{3298
{ch_lk{equ{24,107{{{letter k{3299
{ch_ll{equ{24,108{{{letter l{3300
{ch_lm{equ{24,109{{{letter m{3301
{ch_ln{equ{24,110{{{letter n{3302
{ch_lo{equ{24,111{{{letter o{3303
{ch_lp{equ{24,112{{{letter p{3304
{ch_lq{equ{24,113{{{letter q{3305
{ch_lr{equ{24,114{{{letter r{3306
{ch_ls{equ{24,115{{{letter s{3307
{ch_lt{equ{24,116{{{letter t{3308
{ch_lu{equ{24,117{{{letter u{3309
{ch_lv{equ{24,118{{{letter v{3310
{ch_lw{equ{24,119{{{letter w{3311
{ch_lx{equ{24,120{{{letter x{3312
{ch_ly{equ{24,121{{{letter y{3313
{ch_l_{equ{24,122{{{letter z{3314
*      definitions of codes for digits
{ch_d0{equ{24,48{{{digit 0{3318
{ch_d1{equ{24,49{{{digit 1{3319
{ch_d2{equ{24,50{{{digit 2{3320
{ch_d3{equ{24,51{{{digit 3{3321
{ch_d4{equ{24,52{{{digit 4{3322
{ch_d5{equ{24,53{{{digit 5{3323
{ch_d6{equ{24,54{{{digit 6{3324
{ch_d7{equ{24,55{{{digit 7{3325
{ch_d8{equ{24,56{{{digit 8{3326
{ch_d9{equ{24,57{{{digit 9{3327
{{ejc{{{{{3328
*      definitions of codes for special characters
*      the names of these characters are related to their
*      original representation in the ebcdic set corresponding
*      to the description in standard snobol4 manuals and texts.
{ch_am{equ{24,38{{{keyword operator (ampersand){3336
{ch_as{equ{24,42{{{multiplication symbol (asterisk){3337
{ch_at{equ{24,64{{{cursor position operator (at){3338
{ch_bb{equ{24,60{{{left array bracket (less than){3339
{ch_bl{equ{24,32{{{blank{3340
{ch_br{equ{24,124{{{alternation operator (vertical bar){3341
{ch_cl{equ{24,58{{{goto symbol (colon){3342
{ch_cm{equ{24,44{{{comma{3343
{ch_dl{equ{24,36{{{indirection operator (dollar){3344
{ch_dt{equ{24,46{{{name operator (dot){3345
{ch_dq{equ{24,34{{{double quote{3346
{ch_eq{equ{24,61{{{equal sign{3347
{ch_ex{equ{24,33{{{exponentiation operator (exclm){3348
{ch_mn{equ{24,45{{{minus sign / hyphen{3349
{ch_nm{equ{24,35{{{number sign{3350
{ch_nt{equ{24,126{{{negation operator (not){3351
{ch_pc{equ{24,94{{{percent{3352
{ch_pl{equ{24,43{{{plus sign{3353
{ch_pp{equ{24,40{{{left parenthesis{3354
{ch_rb{equ{24,62{{{right array bracket (grtr than){3355
{ch_rp{equ{24,41{{{right parenthesis{3356
{ch_qu{equ{24,63{{{interrogation operator (question){3357
{ch_sl{equ{24,47{{{slash{3358
{ch_sm{equ{24,59{{{semicolon{3359
{ch_sq{equ{24,39{{{single quote{3360
{ch_u_{equ{24,95{{{special identifier char (underline){3361
{ch_ob{equ{24,91{{{opening bracket{3362
{ch_cb{equ{24,93{{{closing bracket{3363
{{ejc{{{{{3364
*      remaining chars are optional additions to the standards.
*      tab characters - syntactically equivalent to blank
{ch_ht{equ{24,9{{{horizontal tab{3371
*      upper case or shifted case alphabetic chars
{ch_ua{equ{24,65{{{shifted a{3386
{ch_ub{equ{24,66{{{shifted b{3387
{ch_uc{equ{24,67{{{shifted c{3388
{ch_ud{equ{24,68{{{shifted d{3389
{ch_ue{equ{24,69{{{shifted e{3390
{ch_uf{equ{24,70{{{shifted f{3391
{ch_ug{equ{24,71{{{shifted g{3392
{ch_uh{equ{24,72{{{shifted h{3393
{ch_ui{equ{24,73{{{shifted i{3394
{ch_uj{equ{24,74{{{shifted j{3395
{ch_uk{equ{24,75{{{shifted k{3396
{ch_ul{equ{24,76{{{shifted l{3397
{ch_um{equ{24,77{{{shifted m{3398
{ch_un{equ{24,78{{{shifted n{3399
{ch_uo{equ{24,79{{{shifted o{3400
{ch_up{equ{24,80{{{shifted p{3401
{ch_uq{equ{24,81{{{shifted q{3402
{ch_ur{equ{24,82{{{shifted r{3403
{ch_us{equ{24,83{{{shifted s{3404
{ch_ut{equ{24,84{{{shifted t{3405
{ch_uu{equ{24,85{{{shifted u{3406
{ch_uv{equ{24,86{{{shifted v{3407
{ch_uw{equ{24,87{{{shifted w{3408
{ch_ux{equ{24,88{{{shifted x{3409
{ch_uy{equ{24,89{{{shifted y{3410
{ch_uz{equ{24,90{{{shifted z{3411
*      if a delimiter other than ch_cm must be used in
*      the third argument of input(),output() then .ciod should
*      be defined and a parameter supplied for iodel.
{iodel{equ{24,32{{{{3418
{{ejc{{{{{3422
*      data block formats and definitions
*      the following sections describe the detailed format of
*      all possible data blocks in static and dynamic memory.
*      every block has a name of the form xxblk where xx is a
*      unique two character identifier. the first word of every
*      block must contain a pointer to a program location in the
*      interpretor which is immediately preceded by an address
*      constant containing the value bl_xx where xx is the block
*      identifier. this provides a uniform mechanism for
*      distinguishing between the various block types.
*      in some cases, the contents of the first word is constant
*      for a given block type and merely serves as a pointer
*      to the identifying address constant. however, in other
*      cases there are several possibilities for the first
*      word in which case each of the several program entry
*      points must be preceded by the appropriate constant.
*      in each block, some of the fields are relocatable. this
*      means that they may contain a pointer to another block
*      in the dynamic area. (to be more precise, if they contain
*      a pointer within the dynamic area, then it is a pointer
*      to a block). such fields must be modified by the garbage
*      collector (procedure gbcol) whenever blocks are compacted
*      in the dynamic region. the garbage collector (actually
*      procedure gbcpf) requires that all such relocatable
*      fields in a block must be contiguous.
{{ejc{{{{{3453
*      the description format uses the following scheme.
*      1)   block title and two character identifier
*      2)   description of basic use of block and indication
*           of circumstances under which it is constructed.
*      3)   picture of the block format. in these pictures low
*           memory addresses are at the top of the page. fixed
*           length fields are surrounded by i (letter i). fields
*           which are fixed length but whose length is dependent
*           on a configuration parameter are surrounded by *
*           (asterisk). variable length fields are surrounded
*           by / (slash).
*      4)   definition of symbolic offsets to fields in
*           block and of the size of the block if fixed length
*           or of the size of the fixed length fields if the
*           block is variable length.
*           note that some routines such as gbcpf assume
*           certain offsets are equal. the definitions
*           given here enforce this.  make changes to
*           them only with due care.
*      definitions of common offsets
{offs1{equ{24,1{{{{3481
{offs2{equ{24,2{{{{3482
{offs3{equ{24,3{{{{3483
*      5)   detailed comments on the significance and formats
*           of the various fields.
*      the order is alphabetical by identification code.
{{ejc{{{{{3489
*      definitions of block codes
*      this table provides a unique identification code for
*      each separate block type. the first word of a block in
*      the dynamic area always contains the address of a program
*      entry point. the block code is used as the entry point id
*      the order of these codes dictates the order of the table
*      used by the datatype function (scnmt in the constant sec)
*      block codes for accessible datatypes
*      note that real and buffer types are always included, even
*      if they are conditionally excluded elsewhere.  this main-
*      tains block type codes across all versions of spitbol,
*      providing consistancy for external functions.  but note
*      that the bcblk is out of alphabetic order, placed at the
*      end of the list so as not to change the block type
*      ordering in use in existing external functions.
{bl_ar{equ{24,0{{{arblk     array{3510
{bl_cd{equ{24,bl_ar+1{{{cdblk     code{3511
{bl_ex{equ{24,bl_cd+1{{{exblk     expression{3512
{bl_ic{equ{24,bl_ex+1{{{icblk     integer{3513
{bl_nm{equ{24,bl_ic+1{{{nmblk     name{3514
{bl_p0{equ{24,bl_nm+1{{{p0blk     pattern{3515
{bl_p1{equ{24,bl_p0+1{{{p1blk     pattern{3516
{bl_p2{equ{24,bl_p1+1{{{p2blk     pattern{3517
{bl_rc{equ{24,bl_p2+1{{{rcblk     real{3518
{bl_sc{equ{24,bl_rc+1{{{scblk     string{3519
{bl_se{equ{24,bl_sc+1{{{seblk     expression{3520
{bl_tb{equ{24,bl_se+1{{{tbblk     table{3521
{bl_vc{equ{24,bl_tb+1{{{vcblk     array{3522
{bl_xn{equ{24,bl_vc+1{{{xnblk     external{3523
{bl_xr{equ{24,bl_xn+1{{{xrblk     external{3524
{bl_bc{equ{24,bl_xr+1{{{bcblk     buffer{3525
{bl_pd{equ{24,bl_bc+1{{{pdblk     program defined datatype{3526
{bl__d{equ{24,bl_pd+1{{{number of block codes for data{3528
*      other block codes
{bl_tr{equ{24,bl_pd+1{{{trblk{3532
{bl_bf{equ{24,bl_tr+1{{{bfblk{3533
{bl_cc{equ{24,bl_bf+1{{{ccblk{3534
{bl_cm{equ{24,bl_cc+1{{{cmblk{3535
{bl_ct{equ{24,bl_cm+1{{{ctblk{3536
{bl_df{equ{24,bl_ct+1{{{dfblk{3537
{bl_ef{equ{24,bl_df+1{{{efblk{3538
{bl_ev{equ{24,bl_ef+1{{{evblk{3539
{bl_ff{equ{24,bl_ev+1{{{ffblk{3540
{bl_kv{equ{24,bl_ff+1{{{kvblk{3541
{bl_pf{equ{24,bl_kv+1{{{pfblk{3542
{bl_te{equ{24,bl_pf+1{{{teblk{3543
{bl__i{equ{24,0{{{default identification code{3545
{bl__t{equ{24,bl_tr+1{{{code for data or trace block{3546
{bl___{equ{24,bl_te+1{{{number of block codes{3547
{{ejc{{{{{3548
*      field references
*      references to the fields of data blocks are symbolic
*      (i.e. use the symbolic offsets) with the following
*      exceptions.
*      1)   references to the first word are usually not
*           symbolic since they use the (x) operand format.
*      2)   the code which constructs a block is often not
*           symbolic and should be changed if the corresponding
*           block format is modified.
*      3)   the plc and psc instructions imply an offset
*           corresponding to the definition of cfp_f.
*      4)   there are non-symbolic references (easily changed)
*           in the garbage collector (procedures gbcpf, blkln).
*      5)   the fields idval, fargs appear in several blocks
*           and any changes must be made in parallel to all
*           blocks containing the fields. the actual references
*           to these fields are symbolic with the above
*           listed exceptions.
*      6)   several spots in the code assume that the
*           definitions of the fields vrval, teval, trnxt are
*           the same (these are sections of code which search
*           out along a trblk chain from a variable).
*      7)   references to the fields of an array block in the
*           array reference routine arref are non-symbolic.
*      apart from the exceptions listed, references are symbolic
*      as far as possible and modifying the order or number
*      of fields will not require changes.
{{ejc{{{{{3586
*      common fields for function blocks
*      blocks which represent callable functions have two
*      common fields at the start of the block as follows.
*           +------------------------------------+
*           i                fcode               i
*           +------------------------------------+
*           i                fargs               i
*           +------------------------------------+
*           /                                    /
*           /       rest of function block       /
*           /                                    /
*           +------------------------------------+
{fcode{equ{24,0{{{pointer to code for function{3603
{fargs{equ{24,1{{{number of arguments{3604
*      fcode is a pointer to the location in the interpretor
*      program which processes this type of function call.
*      fargs is the expected number of arguments. the actual
*      number of arguments is adjusted to this amount by
*      deleting extra arguments or supplying trailing nulls
*      for missing ones before transferring though fcode.
*      a value of 999 may be used in this field to indicate a
*      variable number of arguments (see svblk field svnar).
*      the block types which follow this scheme are.
*      ffblk                 field function
*      dfblk                 datatype function
*      pfblk                 program defined function
*      efblk                 external loaded function
{{ejc{{{{{3622
*      identification field
*      id   field
*      certain program accessible objects (those which contain
*      other data values and can be copied) are given a unique
*      identification number (see exsid). this id value is an
*      address integer value which is always stored in word two.
{idval{equ{24,1{{{id value field{3634
*      the blocks containing an idval field are.
*      arblk                 array
*      pdblk                 program defined datatype
*      tbblk                 table
*      vcblk                 vector block (array)
*      note that a zero idval means that the block is only
*      half built and should not be dumped (see dumpr).
{{ejc{{{{{3649
*      array block (arblk)
*      an array block represents an array value other than one
*      with one dimension whose lower bound is one (see vcblk).
*      an arblk is built with a call to the functions convert
*      (s_cnv) or array (s_arr).
*           +------------------------------------+
*           i                artyp               i
*           +------------------------------------+
*           i                idval               i
*           +------------------------------------+
*           i                arlen               i
*           +------------------------------------+
*           i                arofs               i
*           +------------------------------------+
*           i                arndm               i
*           +------------------------------------+
*           *                arlbd               *
*           +------------------------------------+
*           *                ardim               *
*           +------------------------------------+
*           *                                    *
*           * above 2 flds repeated for each dim *
*           *                                    *
*           +------------------------------------+
*           i                arpro               i
*           +------------------------------------+
*           /                                    /
*           /                arvls               /
*           /                                    /
*           +------------------------------------+
{{ejc{{{{{3683
*      array block (continued)
{artyp{equ{24,0{{{pointer to dummy routine b_art{3687
{arlen{equ{24,idval+1{{{length of arblk in bytes{3688
{arofs{equ{24,arlen+1{{{offset in arblk to arpro field{3689
{arndm{equ{24,arofs+1{{{number of dimensions{3690
{arlbd{equ{24,arndm+1{{{low bound (first subscript){3691
{ardim{equ{24,arlbd+cfp_i{{{dimension (first subscript){3692
{arlb2{equ{24,ardim+cfp_i{{{low bound (second subscript){3693
{ardm2{equ{24,arlb2+cfp_i{{{dimension (second subscript){3694
{arpro{equ{24,ardim+cfp_i{{{array prototype (one dimension){3695
{arvls{equ{24,arpro+1{{{start of values (one dimension){3696
{arpr2{equ{24,ardm2+cfp_i{{{array prototype (two dimensions){3697
{arvl2{equ{24,arpr2+1{{{start of values (two dimensions){3698
{arsi_{equ{24,arlbd{{{number of standard fields in block{3699
{ardms{equ{24,arlb2-arlbd{{{size of info for one set of bounds{3700
*      the bounds and dimension fields are signed integer
*      values and each occupy cfp_i words in the arblk.
*      the length of an arblk in bytes may not exceed mxlen.
*      this is required to keep name offsets garbage collectable
*      the actual values are arranged in row-wise order and
*      can contain a data pointer or a pointer to a trblk.
{{ejc{{{{{3786
*      code construction block (ccblk)
*      at any one moment there is at most one ccblk into
*      which the compiler is currently storing code (cdwrd).
*           +------------------------------------+
*           i                cctyp               i
*           +------------------------------------+
*           i                cclen               i
*           +------------------------------------+
*           i                ccsln               i
*           +------------------------------------+
*           i                ccuse               i
*           +------------------------------------+
*           /                                    /
*           /                cccod               /
*           /                                    /
*           +------------------------------------+
{cctyp{equ{24,0{{{pointer to dummy routine b_cct{3809
{cclen{equ{24,cctyp+1{{{length of ccblk in bytes{3810
{ccsln{equ{24,cclen+1{{{source line number{3812
{ccuse{equ{24,ccsln+1{{{offset past last used word (bytes){3813
{cccod{equ{24,ccuse+1{{{start of generated code in block{3817
*      the reason that the ccblk is a separate block type from
*      the usual cdblk is that the garbage collector must
*      only process those fields which have been set (see gbcpf)
{{ejc{{{{{3822
*      code block (cdblk)
*      a code block is built for each statement compiled during
*      the initial compilation or by subsequent calls to code.
*           +------------------------------------+
*           i                cdjmp               i
*           +------------------------------------+
*           i                cdstm               i
*           +------------------------------------+
*           i                cdsln               i
*           +------------------------------------+
*           i                cdlen               i
*           +------------------------------------+
*           i                cdfal               i
*           +------------------------------------+
*           /                                    /
*           /                cdcod               /
*           /                                    /
*           +------------------------------------+
{cdjmp{equ{24,0{{{ptr to routine to execute statement{3847
{cdstm{equ{24,cdjmp+1{{{statement number{3848
{cdsln{equ{24,cdstm+1{{{source line number{3850
{cdlen{equ{24,cdsln+1{{{length of cdblk in bytes{3851
{cdfal{equ{24,cdlen+1{{{failure exit (see below){3852
{cdcod{equ{24,cdfal+1{{{executable pseudo-code{3857
{cdsi_{equ{24,cdcod{{{number of standard fields in cdblk{3858
*      cdstm is the statement number of the current statement.
*      cdjmp, cdfal are set as follows.
*      1)   if the failure exit is the next statement
*           cdjmp = b_cds
*           cdfal = ptr to cdblk for next statement
*      2)   if the failure exit is a simple label name
*           cdjmp = b_cds
*           cdfal is a ptr to the vrtra field of the vrblk
*      3)   if there is no failure exit (-nofail mode)
*           cdjmp = b_cds
*           cdfal = o_unf
*      4)   if the failure exit is complex or direct
*           cdjmp = b_cdc
*           cdfal is the offset to the o_gof word
{{ejc{{{{{3883
*      code block (continued)
*      cdcod is the start of the actual code. first we describe
*      the code generated for an expression. in an expression,
*      elements are fetched by name or by value. for example,
*      the binary equal operator fetches its left argument
*      by name and its right argument by value. these two
*      cases generate quite different code and are described
*      separately. first we consider the code by value case.
*      generation of code by value for expressions elements.
*      expression            pointer to exblk or seblk
*      integer constant      pointer to icblk
*      null constant         pointer to nulls
*      pattern               (resulting from preevaluation)
*                            =o_lpt
*                            pointer to p0blk,p1blk or p2blk
*      real constant         pointer to rcblk
*      string constant       pointer to scblk
*      variable              pointer to vrget field of vrblk
*      addition              value code for left operand
*                            value code for right operand
*                            =o_add
*      affirmation           value code for operand
*                            =o_aff
*      alternation           value code for left operand
*                            value code for right operand
*                            =o_alt
*      array reference       (case of one subscript)
*                            value code for array operand
*                            value code for subscript operand
*                            =o_aov
*                            (case of more than one subscript)
*                            value code for array operand
*                            value code for first subscript
*                            value code for second subscript
*                            ...
*                            value code for last subscript
*                            =o_amv
*                            number of subscripts
{{ejc{{{{{3937
*      code block (continued)
*      assignment            (to natural variable)
*                            value code for right operand
*                            pointer to vrsto field of vrblk
*                            (to any other variable)
*                            name code for left operand
*                            value code for right operand
*                            =o_ass
*      compile error         =o_cer
*      complementation       value code for operand
*                            =o_com
*      concatenation         (case of pred func left operand)
*                            value code for left operand
*                            =o_pop
*                            value code for right operand
*                            (all other cases)
*                            value code for left operand
*                            value code for right operand
*                            =o_cnc
*      cursor assignment     name code for operand
*                            =o_cas
*      division              value code for left operand
*                            value code for right operand
*                            =o_dvd
*      exponentiation        value code for left operand
*                            value code for right operand
*                            =o_exp
*      function call         (case of call to system function)
*                            value code for first argument
*                            value code for second argument
*                            ...
*                            value code for last argument
*                            pointer to svfnc field of svblk
{{ejc{{{{{3984
*      code block (continued)
*      function call         (case of non-system function 1 arg)
*                            value code for argument
*                            =o_fns
*                            pointer to vrblk for function
*                            (non-system function, gt 1 arg)
*                            value code for first argument
*                            value code for second argument
*                            ...
*                            value code for last argument
*                            =o_fnc
*                            number of arguments
*                            pointer to vrblk for function
*      immediate assignment  value code for left operand
*                            name code for right operand
*                            =o_ima
*      indirection           value code for operand
*                            =o_inv
*      interrogation         value code for operand
*                            =o_int
*      keyword reference     name code for operand
*                            =o_kwv
*      multiplication        value code for left operand
*                            value code for right operand
*                            =o_mlt
*      name reference        (natural variable case)
*                            pointer to nmblk for name
*                            (all other cases)
*                            name code for operand
*                            =o_nam
*      negation              =o_nta
*                            cdblk offset of o_ntc word
*                            value code for operand
*                            =o_ntb
*                            =o_ntc
{{ejc{{{{{4031
*      code block (continued)
*      pattern assignment    value code for left operand
*                            name code for right operand
*                            =o_pas
*      pattern match         value code for left operand
*                            value code for right operand
*                            =o_pmv
*      pattern replacement   name code for subject
*                            value code for pattern
*                            =o_pmn
*                            value code for replacement
*                            =o_rpl
*      selection             (for first alternative)
*                            =o_sla
*                            cdblk offset to next o_slc word
*                            value code for first alternative
*                            =o_slb
*                            cdblk offset past alternatives
*                            (for subsequent alternatives)
*                            =o_slc
*                            cdblk offset to next o_slc,o_sld
*                            value code for alternative
*                            =o_slb
*                            offset in cdblk past alternatives
*                            (for last alternative)
*                            =o_sld
*                            value code for last alternative
*      subtraction           value code for left operand
*                            value code for right operand
*                            =o_sub
{{ejc{{{{{4070
*      code block (continued)
*      generation of code by name for expression elements.
*      variable              =o_lvn
*                            pointer to vrblk
*      expression            (case of *natural variable)
*                            =o_lvn
*                            pointer to vrblk
*                            (all other cases)
*                            =o_lex
*                            pointer to exblk
*      array reference       (case of one subscript)
*                            value code for array operand
*                            value code for subscript operand
*                            =o_aon
*                            (case of more than one subscript)
*                            value code for array operand
*                            value code for first subscript
*                            value code for second subscript
*                            ...
*                            value code for last subscript
*                            =o_amn
*                            number of subscripts
*      compile error         =o_cer
*      function call         (same code as for value call)
*                            =o_fne
*      indirection           value code for operand
*                            =o_inn
*      keyword reference     name code for operand
*                            =o_kwn
*      any other operand is an error in a name position
*      note that in this description, =o_xxx refers to the
*      generation of a word containing the address of another
*      word which contains the entry point address o_xxx.
{{ejc{{{{{4118
*      code block (continued)
*      now we consider the overall structure of the code block
*      for a statement with possible goto fields.
*      first comes the code for the statement body.
*      the statement body is an expression to be evaluated
*      by value although the value is not actually required.
*      normal value code is generated for the body of the
*      statement except in the case of a pattern match by
*      value, in which case the following is generated.
*                            value code for left operand
*                            value code for right operand
*                            =o_pms
*      next we have the code for the success goto. there are
*      several cases as follows.
*      1)   no success goto  ptr to cdblk for next statement
*      2)   simple label     ptr to vrtra field of vrblk
*      3)   complex goto     (code by name for goto operand)
*                            =o_goc
*      4)   direct goto      (code by value for goto operand)
*                            =o_god
*      following this we generate code for the failure goto if
*      it is direct or if it is complex, simple failure gotos
*      having been handled by an appropriate setting of the
*      cdfal field of the cdblk. the generated code is one
*      of the following.
*      1)   complex fgoto    =o_fif
*                            =o_gof
*                            name code for goto operand
*                            =o_goc
*      2)   direct fgoto     =o_fif
*                            =o_gof
*                            value code for goto operand
*                            =o_god
*      an optimization occurs if the success and failure gotos
*      are identical and either complex or direct. in this case,
*      no code is generated for the success goto and control
*      is allowed to fall into the failure goto on success.
{{ejc{{{{{4169
*      compiler block (cmblk)
*      a compiler block (cmblk) is built by expan to represent
*      one node of a tree structured expression representation.
*           +------------------------------------+
*           i                cmidn               i
*           +------------------------------------+
*           i                cmlen               i
*           +------------------------------------+
*           i                cmtyp               i
*           +------------------------------------+
*           i                cmopn               i
*           +------------------------------------+
*           /           cmvls or cmrop           /
*           /                                    /
*           /                cmlop               /
*           /                                    /
*           +------------------------------------+
{cmidn{equ{24,0{{{pointer to dummy routine b_cmt{4191
{cmlen{equ{24,cmidn+1{{{length of cmblk in bytes{4192
{cmtyp{equ{24,cmlen+1{{{type (c_xxx, see list below){4193
{cmopn{equ{24,cmtyp+1{{{operand pointer (see below){4194
{cmvls{equ{24,cmopn+1{{{operand value pointers (see below){4195
{cmrop{equ{24,cmvls{{{right (only) operator operand{4196
{cmlop{equ{24,cmvls+1{{{left operator operand{4197
{cmsi_{equ{24,cmvls{{{number of standard fields in cmblk{4198
{cmus_{equ{24,cmsi_+1{{{size of unary operator cmblk{4199
{cmbs_{equ{24,cmsi_+2{{{size of binary operator cmblk{4200
{cmar1{equ{24,cmvls+1{{{array subscript pointers{4201
*      the cmopn and cmvls fields are set as follows
*      array reference       cmopn = ptr to array operand
*                            cmvls = ptrs to subscript operands
*      function call         cmopn = ptr to vrblk for function
*                            cmvls = ptrs to argument operands
*      selection             cmopn = zero
*                            cmvls = ptrs to alternate operands
*      unary operator        cmopn = ptr to operator dvblk
*                            cmrop = ptr to operand
*      binary operator       cmopn = ptr to operator dvblk
*                            cmrop = ptr to right operand
*                            cmlop = ptr to left operand
{{ejc{{{{{4220
*      cmtyp is set to indicate the type of expression element
*      as shown by the following table of definitions.
{c_arr{equ{24,0{{{array reference{4225
{c_fnc{equ{24,c_arr+1{{{function call{4226
{c_def{equ{24,c_fnc+1{{{deferred expression (unary *){4227
{c_ind{equ{24,c_def+1{{{indirection (unary _){4228
{c_key{equ{24,c_ind+1{{{keyword reference (unary ampersand){4229
{c_ubo{equ{24,c_key+1{{{undefined binary operator{4230
{c_uuo{equ{24,c_ubo+1{{{undefined unary operator{4231
{c_uo_{equ{24,c_uuo+1{{{test value (=c_uuo+1=c_ubo+2){4232
{c__nm{equ{24,c_uuo+1{{{number of codes for name operands{4233
*      the remaining types indicate expression elements which
*      can only be evaluated by value (not by name).
{c_bvl{equ{24,c_uuo+1{{{binary op with value operands{4238
{c_uvl{equ{24,c_bvl+1{{{unary operator with value operand{4239
{c_alt{equ{24,c_uvl+1{{{alternation (binary bar){4240
{c_cnc{equ{24,c_alt+1{{{concatenation{4241
{c_cnp{equ{24,c_cnc+1{{{concatenation, not pattern match{4242
{c_unm{equ{24,c_cnp+1{{{unary op with name operand{4243
{c_bvn{equ{24,c_unm+1{{{binary op (operands by value, name){4244
{c_ass{equ{24,c_bvn+1{{{assignment{4245
{c_int{equ{24,c_ass+1{{{interrogation{4246
{c_neg{equ{24,c_int+1{{{negation (unary not){4247
{c_sel{equ{24,c_neg+1{{{selection{4248
{c_pmt{equ{24,c_sel+1{{{pattern match{4249
{c_pr_{equ{24,c_bvn{{{last preevaluable code{4251
{c__nv{equ{24,c_pmt+1{{{number of different cmblk types{4252
{{ejc{{{{{4253
*      character table block (ctblk)
*      a character table block is used to hold logical character
*      tables for use with any,notany,span,break,breakx
*      patterns. each character table can be used to store
*      cfp_n distinct tables as bit columns. a bit column
*      allocated for each argument of more than one character
*      in length to one of the above listed pattern primitives.
*           +------------------------------------+
*           i                cttyp               i
*           +------------------------------------+
*           *                                    *
*           *                                    *
*           *                ctchs               *
*           *                                    *
*           *                                    *
*           +------------------------------------+
{cttyp{equ{24,0{{{pointer to dummy routine b_ctt{4274
{ctchs{equ{24,cttyp+1{{{start of character table words{4275
{ctsi_{equ{24,ctchs+cfp_a{{{number of words in ctblk{4276
*      ctchs is cfp_a words long and consists of a one word
*      bit string value for each possible character in the
*      internal alphabet. each of the cfp_n possible bits in
*      a bitstring is used to form a column of bit indicators.
*      a bit is set on if the character is in the table and off
*      if the character is not present.
{{ejc{{{{{4284
*      datatype function block (dfblk)
*      a datatype function is used to control the construction
*      of a program defined datatype object. a call to the
*      system function data builds a dfblk for the datatype name
*      note that these blocks are built in static because pdblk
*      length is got from dflen field.  if dfblk was in dynamic
*      store this would cause trouble during pass two of garbage
*      collection.  scblk referred to by dfnam field is also put
*      in static so that there are no reloc. fields. this cuts
*      garbage collection task appreciably for pdblks which are
*      likely to be present in large numbers.
*           +------------------------------------+
*           i                fcode               i
*           +------------------------------------+
*           i                fargs               i
*           +------------------------------------+
*           i                dflen               i
*           +------------------------------------+
*           i                dfpdl               i
*           +------------------------------------+
*           i                dfnam               i
*           +------------------------------------+
*           /                                    /
*           /                dffld               /
*           /                                    /
*           +------------------------------------+
{dflen{equ{24,fargs+1{{{length of dfblk in bytes{4316
{dfpdl{equ{24,dflen+1{{{length of corresponding pdblk{4317
{dfnam{equ{24,dfpdl+1{{{pointer to scblk for datatype name{4318
{dffld{equ{24,dfnam+1{{{start of vrblk ptrs for field names{4319
{dfflb{equ{24,dffld-1{{{offset behind dffld for field func{4320
{dfsi_{equ{24,dffld{{{number of standard fields in dfblk{4321
*      the fcode field points to the routine b_dfc
*      fargs (the number of arguments) is the number of fields.
{{ejc{{{{{4326
*      dope vector block (dvblk)
*      a dope vector is assembled for each possible operator in
*      the snobol4 language as part of the constant section.
*           +------------------------------------+
*           i                dvopn               i
*           +------------------------------------+
*           i                dvtyp               i
*           +------------------------------------+
*           i                dvlpr               i
*           +------------------------------------+
*           i                dvrpr               i
*           +------------------------------------+
{dvopn{equ{24,0{{{entry address (ptr to o_xxx){4343
{dvtyp{equ{24,dvopn+1{{{type code (c_xxx, see cmblk){4344
{dvlpr{equ{24,dvtyp+1{{{left precedence (llxxx, see below){4345
{dvrpr{equ{24,dvlpr+1{{{right precedence (rrxxx, see below){4346
{dvus_{equ{24,dvlpr+1{{{size of unary operator dv{4347
{dvbs_{equ{24,dvrpr+1{{{size of binary operator dv{4348
{dvubs{equ{24,dvus_+dvbs_{{{size of unop + binop (see scane){4349
*      the contents of the dvtyp field is copied into the cmtyp
*      field of the cmblk for the operator if it is used.
*      the cmopn field of an operator cmblk points to the dvblk
*      itself, providing the required entry address pointer ptr.
*      for normally undefined operators, the dvopn (and cmopn)
*      fields contain a word offset from r_uba of the function
*      block pointer for the operator (instead of o_xxx ptr).
*      for certain special operators, the dvopn field is not
*      required at all and is assembled as zero.
*      the left precedence is used in comparing an operator to
*      the left of some other operator. it therefore governs the
*      precedence of the operator towards its right operand.
*      the right precedence is used in comparing an operator to
*      the right of some other operator. it therefore governs
*      the precedence of the operator towards its left operand.
*      higher precedence values correspond to a tighter binding
*      capability. thus we have the left precedence lower
*      (higher) than the right precedence for right (left)
*      associative binary operators.
*      the left precedence of unary operators is set to an
*      arbitrary high value. the right value is not required and
*      consequently the dvrpr field is omitted for unary ops.
{{ejc{{{{{4379
*      table of operator precedence values
{rrass{equ{24,10{{{right     equal{4383
{llass{equ{24,00{{{left      equal{4384
{rrpmt{equ{24,20{{{right     question mark{4385
{llpmt{equ{24,30{{{left      question mark{4386
{rramp{equ{24,40{{{right     ampersand{4387
{llamp{equ{24,50{{{left      ampersand{4388
{rralt{equ{24,70{{{right     vertical bar{4389
{llalt{equ{24,60{{{left      vertical bar{4390
{rrcnc{equ{24,90{{{right     blank{4391
{llcnc{equ{24,80{{{left      blank{4392
{rrats{equ{24,110{{{right     at{4393
{llats{equ{24,100{{{left      at{4394
{rrplm{equ{24,120{{{right     plus, minus{4395
{llplm{equ{24,130{{{left      plus, minus{4396
{rrnum{equ{24,140{{{right     number{4397
{llnum{equ{24,150{{{left      number{4398
{rrdvd{equ{24,160{{{right     slash{4399
{lldvd{equ{24,170{{{left      slash{4400
{rrmlt{equ{24,180{{{right     asterisk{4401
{llmlt{equ{24,190{{{left      asterisk{4402
{rrpct{equ{24,200{{{right     percent{4403
{llpct{equ{24,210{{{left      percent{4404
{rrexp{equ{24,230{{{right     exclamation{4405
{llexp{equ{24,220{{{left      exclamation{4406
{rrdld{equ{24,240{{{right     dollar, dot{4407
{lldld{equ{24,250{{{left      dollar, dot{4408
{rrnot{equ{24,270{{{right     not{4409
{llnot{equ{24,260{{{left      not{4410
{lluno{equ{24,999{{{left      all unary operators{4411
*      precedences are the same as in btl snobol4 with the
*      following exceptions.
*      1)   binary question mark is lowered and made left assoc-
*           iative to reflect its new use for pattern matching.
*      2)   alternation and concatenation are made right
*           associative for greater efficiency in pattern
*           construction and matching respectively. this change
*           is transparent to the snobol4 programmer.
*      3)   the equal sign has been added as a low precedence
*           operator which is right associative to reflect its
*           more general usage in this version of snobol4.
{{ejc{{{{{4427
*      external function block (efblk)
*      an external function block is used to control the calling
*      of an external function. it is built by a call to load.
*           +------------------------------------+
*           i                fcode               i
*           +------------------------------------+
*           i                fargs               i
*           +------------------------------------+
*           i                eflen               i
*           +------------------------------------+
*           i                efuse               i
*           +------------------------------------+
*           i                efcod               i
*           +------------------------------------+
*           i                efvar               i
*           +------------------------------------+
*           i                efrsl               i
*           +------------------------------------+
*           /                                    /
*           /                eftar               /
*           /                                    /
*           +------------------------------------+
{eflen{equ{24,fargs+1{{{length of efblk in bytes{4454
{efuse{equ{24,eflen+1{{{use count (for opsyn){4455
{efcod{equ{24,efuse+1{{{ptr to code (from sysld){4456
{efvar{equ{24,efcod+1{{{ptr to associated vrblk{4457
{efrsl{equ{24,efvar+1{{{result type (see below){4458
{eftar{equ{24,efrsl+1{{{argument types (see below){4459
{efsi_{equ{24,eftar{{{number of standard fields in efblk{4460
*      the fcode field points to the routine b_efc.
*      efuse is used to keep track of multiple use when opsyn
*      is employed. the function is automatically unloaded
*      when there are no more references to the function.
*      efrsl and eftar are type codes as follows.
*           0                type is unconverted
*           1                type is string
*           2                type is integer
*           3                type is real
*           4                type is file
{{ejc{{{{{4483
*      expression variable block (evblk)
*      in this version of spitbol, an expression can be used in
*      any position which would normally expect a name (for
*      example on the left side of equals or as the right
*      argument of binary dot). this corresponds to the creation
*      of a pseudo-variable which is represented by a pointer to
*      an expression variable block as follows.
*           +------------------------------------+
*           i                evtyp               i
*           +------------------------------------+
*           i                evexp               i
*           +------------------------------------+
*           i                evvar               i
*           +------------------------------------+
{evtyp{equ{24,0{{{pointer to dummy routine b_evt{4502
{evexp{equ{24,evtyp+1{{{pointer to exblk for expression{4503
{evvar{equ{24,evexp+1{{{pointer to trbev dummy trblk{4504
{evsi_{equ{24,evvar+1{{{size of evblk{4505
*      the name of an expression variable is represented by a
*      base pointer to the evblk and an offset of evvar. this
*      value appears to be trapped by the dummy trbev block.
*      note that there is no need to allow for the case of an
*      expression variable which references an seblk since a
*      variable which is of the form *var is equivalent to var.
{{ejc{{{{{4514
*      expression block (exblk)
*      an expression block is built for each expression
*      referenced in a program or created by eval or convert
*      during execution of a program.
*           +------------------------------------+
*           i                extyp               i
*           +------------------------------------+
*           i                exstm               i
*           +------------------------------------+
*           i                exsln               i
*           +------------------------------------+
*           i                exlen               i
*           +------------------------------------+
*           i                exflc               i
*           +------------------------------------+
*           /                                    /
*           /                excod               /
*           /                                    /
*           +------------------------------------+
{extyp{equ{24,0{{{ptr to routine b_exl to load expr{4540
{exstm{equ{24,cdstm{{{stores stmnt no. during evaluation{4541
{exsln{equ{24,exstm+1{{{stores line no. during evaluation{4543
{exlen{equ{24,exsln+1{{{length of exblk in bytes{4544
{exflc{equ{24,exlen+1{{{failure code (=o_fex){4548
{excod{equ{24,exflc+1{{{pseudo-code for expression{4549
{exsi_{equ{24,excod{{{number of standard fields in exblk{4550
*      there are two cases for excod depending on whether the
*      expression can be evaluated by name (see description
*      of cdblk for details of code for expressions).
*      if the expression can be evaluated by name we have.
*                            (code for expr by name)
*                            =o_rnm
*      if the expression can only be evaluated by value.
*                            (code for expr by value)
*                            =o_rvl
{{ejc{{{{{4565
*      field function block (ffblk)
*      a field function block is used to control the selection
*      of a field from a program defined datatype block.
*      a call to data creates an ffblk for each field.
*           +------------------------------------+
*           i                fcode               i
*           +------------------------------------+
*           i                fargs               i
*           +------------------------------------+
*           i                ffdfp               i
*           +------------------------------------+
*           i                ffnxt               i
*           +------------------------------------+
*           i                ffofs               i
*           +------------------------------------+
{ffdfp{equ{24,fargs+1{{{pointer to associated dfblk{4585
{ffnxt{equ{24,ffdfp+1{{{ptr to next ffblk on chain or zero{4586
{ffofs{equ{24,ffnxt+1{{{offset (bytes) to field in pdblk{4587
{ffsi_{equ{24,ffofs+1{{{size of ffblk in words{4588
*      the fcode field points to the routine b_ffc.
*      fargs always contains one.
*      ffdfp is used to verify that the correct program defined
*      datatype is being accessed by this call.
*      ffdfp is non-reloc. because dfblk is in static
*      ffofs is used to select the appropriate field. note that
*      it is an actual offset (not a field number)
*      ffnxt is used to point to the next ffblk of the same name
*      in the case where there are several fields of the same
*      name for different datatypes. zero marks the end of chain
{{ejc{{{{{4604
*      integer constant block (icblk)
*      an icblk is created for every integer referenced or
*      created by a program. note however that certain internal
*      integer values are stored as addresses (e.g. the length
*      field in a string constant block)
*           +------------------------------------+
*           i                icget               i
*           +------------------------------------+
*           *                icval               *
*           +------------------------------------+
{icget{equ{24,0{{{ptr to routine b_icl to load int{4619
{icval{equ{24,icget+1{{{integer value{4620
{icsi_{equ{24,icval+cfp_i{{{size of icblk{4621
*      the length of the icval field is cfp_i.
{{ejc{{{{{4624
*      keyword variable block (kvblk)
*      a kvblk is used to represent a keyword pseudo-variable.
*      a kvblk is built for each keyword reference (kwnam).
*           +------------------------------------+
*           i                kvtyp               i
*           +------------------------------------+
*           i                kvvar               i
*           +------------------------------------+
*           i                kvnum               i
*           +------------------------------------+
{kvtyp{equ{24,0{{{pointer to dummy routine b_kvt{4639
{kvvar{equ{24,kvtyp+1{{{pointer to dummy block trbkv{4640
{kvnum{equ{24,kvvar+1{{{keyword number{4641
{kvsi_{equ{24,kvnum+1{{{size of kvblk{4642
*      the name of a keyword variable is represented by a
*      base pointer to the kvblk and an offset of kvvar. the
*      value appears to be trapped by the pointer to trbkv.
{{ejc{{{{{4647
*      name block (nmblk)
*      a name block is used wherever a name must be stored as
*      a value following use of the unary dot operator.
*           +------------------------------------+
*           i                nmtyp               i
*           +------------------------------------+
*           i                nmbas               i
*           +------------------------------------+
*           i                nmofs               i
*           +------------------------------------+
{nmtyp{equ{24,0{{{ptr to routine b_nml to load name{4662
{nmbas{equ{24,nmtyp+1{{{base pointer for variable{4663
{nmofs{equ{24,nmbas+1{{{offset for variable{4664
{nmsi_{equ{24,nmofs+1{{{size of nmblk{4665
*      the actual field representing the contents of the name
*      is found nmofs bytes past the address in nmbas.
*      the name is split into base and offset form to avoid
*      creation of a pointer into the middle of a block which
*      could not be handled properly by the garbage collector.
*      a name may be built for any variable (see section on
*      representations of variables) this includes the
*      cases of pseudo-variables.
{{ejc{{{{{4677
*      pattern block, no parameters (p0blk)
*      a p0blk is used to represent pattern nodes which do
*      not require the use of any parameter values.
*           +------------------------------------+
*           i                pcode               i
*           +------------------------------------+
*           i                pthen               i
*           +------------------------------------+
{pcode{equ{24,0{{{ptr to match routine (p_xxx){4690
{pthen{equ{24,pcode+1{{{pointer to subsequent node{4691
{pasi_{equ{24,pthen+1{{{size of p0blk{4692
*      pthen points to the pattern block for the subsequent
*      node to be matched. this is a pointer to the pattern
*      block ndnth if there is no subsequent (end of pattern)
*      pcode is a pointer to the match routine for the node.
{{ejc{{{{{4699
*      pattern block (one parameter)
*      a p1blk is used to represent pattern nodes which
*      require one parameter value.
*           +------------------------------------+
*           i                pcode               i
*           +------------------------------------+
*           i                pthen               i
*           +------------------------------------+
*           i                parm1               i
*           +------------------------------------+
{parm1{equ{24,pthen+1{{{first parameter value{4714
{pbsi_{equ{24,parm1+1{{{size of p1blk in words{4715
*      see p0blk for definitions of pcode, pthen
*      parm1 contains a parameter value used in matching the
*      node. for example, in a len pattern, it is the integer
*      argument to len. the details of the use of the parameter
*      field are included in the description of the individual
*      match routines. parm1 is always an address pointer which
*      is processed by the garbage collector.
{{ejc{{{{{4725
*      pattern block (two parameters)
*      a p2blk is used to represent pattern nodes which
*      require two parameter values.
*           +------------------------------------+
*           i                pcode               i
*           +------------------------------------+
*           i                pthen               i
*           +------------------------------------+
*           i                parm1               i
*           +------------------------------------+
*           i                parm2               i
*           +------------------------------------+
{parm2{equ{24,parm1+1{{{second parameter value{4742
{pcsi_{equ{24,parm2+1{{{size of p2blk in words{4743
*      see p1blk for definitions of pcode, pthen, parm1
*      parm2 is a parameter which performs the same sort of
*      function as parm1 (see description of p1blk).
*      parm2 is a non-relocatable field and is not
*      processed by the garbage collector. accordingly, it may
*      not contain a pointer to a block in dynamic memory.
{{ejc{{{{{4753
*      program-defined datatype block
*      a pdblk represents the data item formed by a call to a
*      datatype function as defined by the system function data.
*           +------------------------------------+
*           i                pdtyp               i
*           +------------------------------------+
*           i                idval               i
*           +------------------------------------+
*           i                pddfp               i
*           +------------------------------------+
*           /                                    /
*           /                pdfld               /
*           /                                    /
*           +------------------------------------+
{pdtyp{equ{24,0{{{ptr to dummy routine b_pdt{4772
{pddfp{equ{24,idval+1{{{ptr to associated dfblk{4773
{pdfld{equ{24,pddfp+1{{{start of field value pointers{4774
{pdfof{equ{24,dffld-pdfld{{{difference in offset to field ptrs{4775
{pdsi_{equ{24,pdfld{{{size of standard fields in pdblk{4776
{pddfs{equ{24,dfsi_-pdsi_{{{difference in dfblk, pdblk sizes{4777
*      the pddfp pointer may be used to determine the datatype
*      and the names of the fields if required. the dfblk also
*      contains the length of the pdblk in bytes (field dfpdl).
*      pddfp is non-reloc. because dfblk is in static
*      pdfld values are stored in order from left to right.
*      they contain values or pointers to trblk chains.
{{ejc{{{{{4786
*      program defined function block (pfblk)
*      a pfblk is created for each call to the define function
*      and a pointer to the pfblk placed in the proper vrblk.
*           +------------------------------------+
*           i                fcode               i
*           +------------------------------------+
*           i                fargs               i
*           +------------------------------------+
*           i                pflen               i
*           +------------------------------------+
*           i                pfvbl               i
*           +------------------------------------+
*           i                pfnlo               i
*           +------------------------------------+
*           i                pfcod               i
*           +------------------------------------+
*           i                pfctr               i
*           +------------------------------------+
*           i                pfrtr               i
*           +------------------------------------+
*           /                                    /
*           /                pfarg               /
*           /                                    /
*           +------------------------------------+
{pflen{equ{24,fargs+1{{{length of pfblk in bytes{4815
{pfvbl{equ{24,pflen+1{{{pointer to vrblk for function name{4816
{pfnlo{equ{24,pfvbl+1{{{number of locals{4817
{pfcod{equ{24,pfnlo+1{{{ptr to vrblk for entry label{4818
{pfctr{equ{24,pfcod+1{{{trblk ptr if call traced else 0{4819
{pfrtr{equ{24,pfctr+1{{{trblk ptr if return traced else 0{4820
{pfarg{equ{24,pfrtr+1{{{vrblk ptrs for arguments and locals{4821
{pfagb{equ{24,pfarg-1{{{offset behind pfarg for arg, local{4822
{pfsi_{equ{24,pfarg{{{number of standard fields in pfblk{4823
*      the fcode field points to the routine b_pfc.
*      pfarg is stored in the following order.
*           arguments (left to right)
*           locals (left to right)
{{ejc{{{{{4833
*      real constant block (rcblk)
*      an rcblk is created for every real referenced or
*      created by a program.
*           +------------------------------------+
*           i                rcget               i
*           +------------------------------------+
*           *                rcval               *
*           +------------------------------------+
{rcget{equ{24,0{{{ptr to routine b_rcl to load real{4846
{rcval{equ{24,rcget+1{{{real value{4847
{rcsi_{equ{24,rcval+cfp_r{{{size of rcblk{4848
*      the length of the rcval field is cfp_r.
{{ejc{{{{{4852
*      string constant block (scblk)
*      an scblk is built for every string referenced or created
*      by a program.
*           +------------------------------------+
*           i                scget               i
*           +------------------------------------+
*           i                sclen               i
*           +------------------------------------+
*           /                                    /
*           /                schar               /
*           /                                    /
*           +------------------------------------+
{scget{equ{24,0{{{ptr to routine b_scl to load string{4869
{sclen{equ{24,scget+1{{{length of string in characters{4870
{schar{equ{24,sclen+1{{{characters of string{4871
{scsi_{equ{24,schar{{{size of standard fields in scblk{4872
*      the characters of the string are stored left justified.
*      the final word is padded on the right with zeros.
*      (i.e. the character whose internal code is zero).
*      the value of sclen may not exceed mxlen. this ensures
*      that character offsets (e.g. the pattern match cursor)
*      can be correctly processed by the garbage collector.
*      note that the offset to the characters of the string
*      is given in bytes by cfp_f and that this value is
*      automatically allowed for in plc, psc.
*      note that for a spitbol scblk, the value of cfp_f
*      is given by cfp_b*schar.
{{ejc{{{{{4887
*      simple expression block (seblk)
*      an seblk is used to represent an expression of the form
*      *(natural variable). all other expressions are exblks.
*           +------------------------------------+
*           i                setyp               i
*           +------------------------------------+
*           i                sevar               i
*           +------------------------------------+
{setyp{equ{24,0{{{ptr to routine b_sel to load expr{4900
{sevar{equ{24,setyp+1{{{ptr to vrblk for variable{4901
{sesi_{equ{24,sevar+1{{{length of seblk in words{4902
{{ejc{{{{{4903
*      standard variable block (svblk)
*      an svblk is assembled in the constant section for each
*      variable which satisfies one of the following conditions.
*      1)   it is the name of a system function
*      2)   it has an initial value
*      3)   it has a keyword association
*      4)   it has a standard i/o association
*      6)   it has a standard label association
*      if vrblks are constructed for any of these variables,
*      then the vrsvp field points to the svblk (see vrblk)
*           +------------------------------------+
*           i                svbit               i
*           +------------------------------------+
*           i                svlen               i
*           +------------------------------------+
*           /                svchs               /
*           +------------------------------------+
*           i                svknm               i
*           +------------------------------------+
*           i                svfnc               i
*           +------------------------------------+
*           i                svnar               i
*           +------------------------------------+
*           i                svlbl               i
*           +------------------------------------+
*           i                svval               i
*           +------------------------------------+
{{ejc{{{{{4936
*      standard variable block (continued)
{svbit{equ{24,0{{{bit string indicating attributes{4940
{svlen{equ{24,1{{{(=sclen) length of name in chars{4941
{svchs{equ{24,2{{{(=schar) characters of name{4942
{svsi_{equ{24,2{{{number of standard fields in svblk{4943
{svpre{equ{24,1{{{set if preevaluation permitted{4944
{svffc{equ{24,svpre+svpre{{{set on if fast call permitted{4945
{svckw{equ{24,svffc+svffc{{{set on if keyword value constant{4946
{svprd{equ{24,svckw+svckw{{{set on if predicate function{4947
{svnbt{equ{24,4{{{number of bits to right of svknm{4948
{svknm{equ{24,svprd+svprd{{{set on if keyword association{4949
{svfnc{equ{24,svknm+svknm{{{set on if system function{4950
{svnar{equ{24,svfnc+svfnc{{{set on if system function{4951
{svlbl{equ{24,svnar+svnar{{{set on if system label{4952
{svval{equ{24,svlbl+svlbl{{{set on if predefined value{4953
*      note that the last five bits correspond in order
*      to the fields which are present (see procedure gtnvr).
*      the following definitions are used in the svblk table
{svfnf{equ{24,svfnc+svnar{{{function with no fast call{4960
{svfnn{equ{24,svfnf+svffc{{{function with fast call, no preeval{4961
{svfnp{equ{24,svfnn+svpre{{{function allowing preevaluation{4962
{svfpr{equ{24,svfnn+svprd{{{predicate function{4963
{svfnk{equ{24,svfnn+svknm{{{no preeval func + keyword{4964
{svkwv{equ{24,svknm+svval{{{keyword + value{4965
{svkwc{equ{24,svckw+svknm{{{keyword with constant value{4966
{svkvc{equ{24,svkwv+svckw{{{constant keyword + value{4967
{svkvl{equ{24,svkvc+svlbl{{{constant keyword + value + label{4968
{svfpk{equ{24,svfnp+svkvc{{{preeval fcn + const keywd + val{4969
*      the svpre bit allows the compiler to preevaluate a call
*      to the associated system function if all the arguments
*      are themselves constants. functions in this category
*      must have no side effects and must never cause failure.
*      the call may generate an error condition.
*      the svffc bit allows the compiler to generate the special
*      fast call after adjusting the number of arguments. only
*      the item and apply functions fall outside this category.
*      the svckw bit is set if the associated keyword value is
*      a constant, thus allowing preevaluation for a value call.
*      the svprd bit is set on for all predicate functions to
*      enable the special concatenation code optimization.
{{ejc{{{{{4986
*      svblk (continued)
*      svknm                 keyword number
*           svknm is present only for a standard keyword assoc.
*           it contains a keyword number as defined by the
*           keyword number table given later on.
*      svfnc                 system function pointer
*           svfnc is present only for a system function assoc.
*           it is a pointer to the actual code for the system
*           function. the generated code for a fast call is a
*           pointer to the svfnc field of the svblk for the
*           function. the vrfnc field of the vrblk points to
*           this same field, in which case, it serves as the
*           fcode field for the function call.
*      svnar                 number of function arguments
*           svnar is present only for a system function assoc.
*           it is the number of arguments required for a call
*           to the system function. the compiler uses this
*           value to adjust the number of arguments in a fast
*           call and in the case of a function called through
*           the vrfnc field of the vrblk, the svnar field
*           serves as the fargs field for o_fnc. a special
*           case occurs if this value is set to 999. this is
*           used to indicate that the function has a variable
*           number of arguments and causes o_fnc to pass control
*           without adjusting the argument count. the only
*           predefined functions using this are apply and item.
*      svlbl                 system label pointer
*           svlbl is present only for a standard label assoc.
*           it is a pointer to a system label routine (l_xxx).
*           the vrlbl field of the corresponding vrblk points to
*           the svlbl field of the svblk.
*      svval                 system value pointer
*           svval is present only for a standard value.
*           it is a pointer to the pattern node (ndxxx) which
*           is the standard initial value of the variable.
*           this value is copied to the vrval field of the vrblk
{{ejc{{{{{5034
*      svblk (continued)
*      keyword number table
*      the following table gives symbolic names for keyword
*      numbers. these values are stored in the svknm field of
*      svblks and in the kvnum field of kvblks. see also
*      procedures asign, acess and kwnam.
*      unprotected keywords with one word integer values
{k_abe{equ{24,0{{{abend{5047
{k_anc{equ{24,k_abe+cfp_b{{{anchor{5048
{k_cod{equ{24,k_anc+cfp_b{{{code{5053
{k_com{equ{24,k_cod+cfp_b{{{compare{5056
{k_dmp{equ{24,k_com+cfp_b{{{dump{5057
{k_erl{equ{24,k_dmp+cfp_b{{{errlimit{5061
{k_ert{equ{24,k_erl+cfp_b{{{errtype{5062
{k_ftr{equ{24,k_ert+cfp_b{{{ftrace{5063
{k_fls{equ{24,k_ftr+cfp_b{{{fullscan{5064
{k_inp{equ{24,k_fls+cfp_b{{{input{5065
{k_mxl{equ{24,k_inp+cfp_b{{{maxlength{5066
{k_oup{equ{24,k_mxl+cfp_b{{{output{5067
{k_pfl{equ{24,k_oup+cfp_b{{{profile{5071
{k_tra{equ{24,k_pfl+cfp_b{{{trace{5072
{k_trm{equ{24,k_tra+cfp_b{{{trim{5074
*      protected keywords with one word integer values
{k_fnc{equ{24,k_trm+cfp_b{{{fnclevel{5078
{k_lst{equ{24,k_fnc+cfp_b{{{lastno{5079
{k_lln{equ{24,k_lst+cfp_b{{{lastline{5081
{k_lin{equ{24,k_lln+cfp_b{{{line{5082
{k_stn{equ{24,k_lin+cfp_b{{{stno{5083
*      keywords with constant pattern values
{k_abo{equ{24,k_stn+cfp_b{{{abort{5090
{k_arb{equ{24,k_abo+pasi_{{{arb{5091
{k_bal{equ{24,k_arb+pasi_{{{bal{5092
{k_fal{equ{24,k_bal+pasi_{{{fail{5093
{k_fen{equ{24,k_fal+pasi_{{{fence{5094
{k_rem{equ{24,k_fen+pasi_{{{rem{5095
{k_suc{equ{24,k_rem+pasi_{{{succeed{5096
{{ejc{{{{{5097
*      keyword number table (continued)
*      special keywords
{k_alp{equ{24,k_suc+1{{{alphabet{5103
{k_rtn{equ{24,k_alp+1{{{rtntype{5104
{k_stc{equ{24,k_rtn+1{{{stcount{5105
{k_etx{equ{24,k_stc+1{{{errtext{5106
{k_fil{equ{24,k_etx+1{{{file{5108
{k_lfl{equ{24,k_fil+1{{{lastfile{5109
{k_stl{equ{24,k_lfl+1{{{stlimit{5110
{k_lcs{equ{24,k_stl+1{{{lcase{5115
{k_ucs{equ{24,k_lcs+1{{{ucase{5116
*      relative offsets of special keywords
{k__al{equ{24,k_alp-k_alp{{{alphabet{5121
{k__rt{equ{24,k_rtn-k_alp{{{rtntype{5122
{k__sc{equ{24,k_stc-k_alp{{{stcount{5123
{k__et{equ{24,k_etx-k_alp{{{errtext{5124
{k__fl{equ{24,k_fil-k_alp{{{file{5126
{k__lf{equ{24,k_lfl-k_alp{{{lastfile{5127
{k__sl{equ{24,k_stl-k_alp{{{stlimit{5129
{k__lc{equ{24,k_lcs-k_alp{{{lcase{5131
{k__uc{equ{24,k_ucs-k_alp{{{ucase{5132
{k__n_{equ{24,k__uc+1{{{number of special cases{5133
*      symbols used in asign and acess procedures
{k_p__{equ{24,k_fnc{{{first protected keyword{5140
{k_v__{equ{24,k_abo{{{first keyword with constant value{5141
{k_s__{equ{24,k_alp{{{first keyword with special acess{5142
{{ejc{{{{{5143
*      format of a table block (tbblk)
*      a table block is used to represent a table value.
*      it is built by a call to the table or convert functions.
*           +------------------------------------+
*           i                tbtyp               i
*           +------------------------------------+
*           i                idval               i
*           +------------------------------------+
*           i                tblen               i
*           +------------------------------------+
*           i                tbinv               i
*           +------------------------------------+
*           /                                    /
*           /                tbbuk               /
*           /                                    /
*           +------------------------------------+
{tbtyp{equ{24,0{{{pointer to dummy routine b_tbt{5164
{tblen{equ{24,offs2{{{length of tbblk in bytes{5165
{tbinv{equ{24,offs3{{{default initial lookup value{5166
{tbbuk{equ{24,tbinv+1{{{start of hash bucket pointers{5167
{tbsi_{equ{24,tbbuk{{{size of standard fields in tbblk{5168
{tbnbk{equ{24,11{{{default no. of buckets{5169
*      the table block is a hash table which points to chains
*      of table element blocks representing the elements
*      in the table which hash into the same bucket.
*      tbbuk entries either point to the first teblk on the
*      chain or they point to the tbblk itself to indicate the
*      end of the chain.
{{ejc{{{{{5178
*      table element block (teblk)
*      a table element is used to represent a single entry in
*      a table (see description of tbblk format for hash table)
*           +------------------------------------+
*           i                tetyp               i
*           +------------------------------------+
*           i                tesub               i
*           +------------------------------------+
*           i                teval               i
*           +------------------------------------+
*           i                tenxt               i
*           +------------------------------------+
{tetyp{equ{24,0{{{pointer to dummy routine b_tet{5195
{tesub{equ{24,tetyp+1{{{subscript value{5196
{teval{equ{24,tesub+1{{{(=vrval) table element value{5197
{tenxt{equ{24,teval+1{{{link to next teblk{5198
*      see s_cnv where relation is assumed with tenxt and tbbuk
{tesi_{equ{24,tenxt+1{{{size of teblk in words{5200
*      tenxt points to the next teblk on the hash chain from the
*      tbbuk chain for this hash index. at the end of the chain,
*      tenxt points back to the start of the tbblk.
*      teval contains a data pointer or a trblk pointer.
*      tesub contains a data pointer.
{{ejc{{{{{5209
*      trap block (trblk)
*      a trap block is used to represent a trace or input or
*      output association in response to a call to the trace
*      input or output system functions. see below for details
*           +------------------------------------+
*           i                tridn               i
*           +------------------------------------+
*           i                trtyp               i
*           +------------------------------------+
*           i  trval or trlbl or trnxt or trkvr  i
*           +------------------------------------+
*           i       trtag or trter or trtrf      i
*           +------------------------------------+
*           i            trfnc or trfpt          i
*           +------------------------------------+
{tridn{equ{24,0{{{pointer to dummy routine b_trt{5229
{trtyp{equ{24,tridn+1{{{trap type code{5230
{trval{equ{24,trtyp+1{{{value of trapped variable (=vrval){5231
{trnxt{equ{24,trval{{{ptr to next trblk on trblk chain{5232
{trlbl{equ{24,trval{{{ptr to actual label (traced label){5233
{trkvr{equ{24,trval{{{vrblk pointer for keyword trace{5234
{trtag{equ{24,trval+1{{{trace tag{5235
{trter{equ{24,trtag{{{ptr to terminal vrblk or null{5236
{trtrf{equ{24,trtag{{{ptr to trblk holding fcblk ptr{5237
{trfnc{equ{24,trtag+1{{{trace function vrblk (zero if none){5238
{trfpt{equ{24,trfnc{{{fcblk ptr for sysio{5239
{trsi_{equ{24,trfnc+1{{{number of words in trblk{5240
{trtin{equ{24,0{{{trace type for input association{5242
{trtac{equ{24,trtin+1{{{trace type for access trace{5243
{trtvl{equ{24,trtac+1{{{trace type for value trace{5244
{trtou{equ{24,trtvl+1{{{trace type for output association{5245
{trtfc{equ{24,trtou+1{{{trace type for fcblk identification{5246
{{ejc{{{{{5247
*      trap block (continued)
*      variable input association
*           the value field of the variable points to a trblk
*           instead of containing the data value. in the case
*           of a natural variable, the vrget and vrsto fields
*           contain =b_vra and =b_vrv to activate the check.
*           trtyp is set to trtin
*           trnxt points to next trblk or trval has variable val
*           trter is a pointer to svblk if association is
*           for input, terminal, else it is null.
*           trtrf points to the trap block which in turn points
*           to an fcblk used for i/o association.
*           trfpt is the fcblk ptr returned by sysio.
*      variable access trace association
*           the value field of the variable points to a trblk
*           instead of containing the data value. in the case
*           of a natural variable, the vrget and vrsto fields
*           contain =b_vra and =b_vrv to activate the check.
*           trtyp is set to trtac
*           trnxt points to next trblk or trval has variable val
*           trtag is the trace tag (0 if none)
*           trfnc is the trace function vrblk ptr (0 if none)
*      variable value trace association
*           the value field of the variable points to a trblk
*           instead of containing the data value. in the case
*           of a natural variable, the vrget and vrsto fields
*           contain =b_vra and =b_vrv to activate the check.
*           trtyp is set to trtvl
*           trnxt points to next trblk or trval has variable val
*           trtag is the trace tag (0 if none)
*           trfnc is the trace function vrblk ptr (0 if none)
{{ejc{{{{{5289
*      trap block (continued)
*      variable output association
*           the value field of the variable points to a trblk
*           instead of containing the data value. in the case
*           of a natural variable, the vrget and vrsto fields
*           contain =b_vra and =b_vrv to activate the check.
*           trtyp is set to trtou
*           trnxt points to next trblk or trval has variable val
*           trter is a pointer to svblk if association is
*           for output, terminal, else it is null.
*           trtrf points to the trap block which in turn points
*           to an fcblk used for i/o association.
*           trfpt is the fcblk ptr returned by sysio.
*      function call trace
*           the pfctr field of the corresponding pfblk is set
*           to point to a trblk.
*           trtyp is set to trtin
*           trnxt is zero
*           trtag is the trace tag (0 if none)
*           trfnc is the trace function vrblk ptr (0 if none)
*      function return trace
*           the pfrtr field of the corresponding pfblk is set
*           to point to a trblk
*           trtyp is set to trtin
*           trnxt is zero
*           trtag is the trace tag (0 if none)
*           trfnc is the trace function vrblk ptr (0 if none)
*      label trace
*           the vrlbl of the vrblk for the label is
*           changed to point to a trblk and the vrtra field is
*           set to b_vrt to activate the check.
*           trtyp is set to trtin
*           trlbl points to the actual label (cdblk) value
*           trtag is the trace tag (0 if none)
*           trfnc is the trace function vrblk ptr (0 if none)
{{ejc{{{{{5337
*      trap block (continued)
*      keyword trace
*           keywords which can be traced possess a unique
*           location which is zero if there is no trace and
*           points to a trblk if there is a trace. the locations
*           are as follows.
*           r_ert            errtype
*           r_fnc            fnclevel
*           r_stc            stcount
*           the format of the trblk is as follows.
*           trtyp is set to trtin
*           trkvr is a pointer to the vrblk for the keyword
*           trtag is the trace tag (0 if none)
*           trfnc is the trace function vrblk ptr (0 if none)
*      input/output file arg1 trap block
*           the value field of the variable points to a trblk
*           instead of containing the data value. in the case of
*           a natural variable, the vrget and vrsto fields
*           contain =b_vra and =b_vrv. this trap block is used
*           to hold a pointer to the fcblk which an
*           implementation may request to hold information
*           about a file.
*           trtyp is set to trtfc
*           trnext points to next trblk or trval is variable val
*           trfnm is 0
*           trfpt is the fcblk pointer.
*      note that when multiple traps are set on a variable
*      the order is in ascending value of trtyp field.
*      input association (if present)
*      access trace (if present)
*      value trace (if present)
*      output association (if present)
*      the actual value of the variable is stored in the trval
*      field of the last trblk on the chain.
*      this implementation does not permit trace or i/o
*      associations to any of the pseudo-variables.
{{ejc{{{{{5387
*      vector block (vcblk)
*      a vcblk is used to represent an array value which has
*      one dimension whose lower bound is one. all other arrays
*      are represented by arblks. a vcblk is created by the
*      system function array (s_arr) when passed an integer arg.
*           +------------------------------------+
*           i                vctyp               i
*           +------------------------------------+
*           i                idval               i
*           +------------------------------------+
*           i                vclen               i
*           +------------------------------------+
*           i                vcvls               i
*           +------------------------------------+
{vctyp{equ{24,0{{{pointer to dummy routine b_vct{5406
{vclen{equ{24,offs2{{{length of vcblk in bytes{5407
{vcvls{equ{24,offs3{{{start of vector values{5408
{vcsi_{equ{24,vcvls{{{size of standard fields in vcblk{5409
{vcvlb{equ{24,vcvls-1{{{offset one word behind vcvls{5410
{vctbd{equ{24,tbsi_-vcsi_{{{difference in sizes - see prtvl{5411
*      vcvls are either data pointers or trblk pointers
*      the dimension can be deduced from vclen.
{{ejc{{{{{5416
*      variable block (vrblk)
*      a variable block is built in the static memory area
*      for every variable referenced or created by a program.
*      the order of fields is assumed in the model vrblk stnvr.
*      note that since these blocks only occur in the static
*      region, it is permissible to point to any word in
*      the block and this is used to provide three distinct
*      access points from the generated code as follows.
*      1)   point to vrget (first word of vrblk) to load the
*           value of the variable onto the main stack.
*      2)   point to vrsto (second word of vrblk) to store the
*           top stack element as the value of the variable.
*      3)   point to vrtra (fourth word of vrblk) to jump to
*           the label associated with the variable name.
*           +------------------------------------+
*           i                vrget               i
*           +------------------------------------+
*           i                vrsto               i
*           +------------------------------------+
*           i                vrval               i
*           +------------------------------------+
*           i                vrtra               i
*           +------------------------------------+
*           i                vrlbl               i
*           +------------------------------------+
*           i                vrfnc               i
*           +------------------------------------+
*           i                vrnxt               i
*           +------------------------------------+
*           i                vrlen               i
*           +------------------------------------+
*           /                                    /
*           /            vrchs = vrsvp           /
*           /                                    /
*           +------------------------------------+
{{ejc{{{{{5459
*      variable block (continued)
{vrget{equ{24,0{{{pointer to routine to load value{5463
{vrsto{equ{24,vrget+1{{{pointer to routine to store value{5464
{vrval{equ{24,vrsto+1{{{variable value{5465
{vrvlo{equ{24,vrval-vrsto{{{offset to value from store field{5466
{vrtra{equ{24,vrval+1{{{pointer to routine to jump to label{5467
{vrlbl{equ{24,vrtra+1{{{pointer to code for label{5468
{vrlbo{equ{24,vrlbl-vrtra{{{offset to label from transfer field{5469
{vrfnc{equ{24,vrlbl+1{{{pointer to function block{5470
{vrnxt{equ{24,vrfnc+1{{{pointer to next vrblk on hash chain{5471
{vrlen{equ{24,vrnxt+1{{{length of name (or zero){5472
{vrchs{equ{24,vrlen+1{{{characters of name (vrlen gt 0){5473
{vrsvp{equ{24,vrlen+1{{{ptr to svblk (vrlen eq 0){5474
{vrsi_{equ{24,vrchs+1{{{number of standard fields in vrblk{5475
{vrsof{equ{24,vrlen-sclen{{{offset to dummy scblk for name{5476
{vrsvo{equ{24,vrsvp-vrsof{{{pseudo-offset to vrsvp field{5477
*      vrget = b_vrl if not input associated or access traced
*      vrget = b_vra if input associated or access traced
*      vrsto = b_vrs if not output associated or value traced
*      vrsto = b_vrv if output associated or value traced
*      vrsto = b_vre if value is protected pattern value
*      vrval points to the appropriate value unless the
*      variable is i/o/trace associated in which case, vrval
*      points to an appropriate trblk (trap block) chain.
*      vrtra = b_vrg if the label is not traced
*      vrtra = b_vrt if the label is traced
*      vrlbl points to a cdblk if there is a label
*      vrlbl points to the svblk svlbl field for a system label
*      vrlbl points to stndl for an undefined label
*      vrlbl points to a trblk if the label is traced
*      vrfnc points to a ffblk for a field function
*      vrfnc points to a dfblk for a datatype function
*      vrfnc points to a pfblk for a program defined function
*      vrfnc points to a efblk for an external loaded function
*      vrfnc points to svfnc (svblk) for a system function
*      vrfnc points to stndf if the function is undefined
*      vrnxt points to the next vrblk on this chain unless
*      this is the end of the chain in which case it is zero.
*      vrlen is the name length for a non-system variable.
*      vrlen is zero for a system variable.
*      vrchs is the name (ljrz) if vrlen is non-zero.
*      vrsvp is a ptr to the svblk if vrlen is zero.
{{ejc{{{{{5513
*      format of a non-relocatable external block (xnblk)
*      an xnblk is a block representing an unknown (external)
*      data value. the block contains no pointers to other
*      relocatable blocks. an xnblk is used by external function
*      processing or possibly for system i/o routines etc.
*      the macro-system itself does not use xnblks.
*      this type of block may be used as a file control block.
*      see sysfc,sysin,sysou,s_inp,s_oup for details.
*           +------------------------------------+
*           i                xntyp               i
*           +------------------------------------+
*           i                xnlen               i
*           +------------------------------------+
*           /                                    /
*           /                xndta               /
*           /                                    /
*           +------------------------------------+
{xntyp{equ{24,0{{{pointer to dummy routine b_xnt{5535
{xnlen{equ{24,xntyp+1{{{length of xnblk in bytes{5536
{xndta{equ{24,xnlen+1{{{data words{5537
{xnsi_{equ{24,xndta{{{size of standard fields in xnblk{5538
*      note that the term non-relocatable refers to the contents
*      and not the block itself. an xnblk can be moved around if
*      it is built in the dynamic memory area.
{{ejc{{{{{5543
*      relocatable external block (xrblk)
*      an xrblk is a block representing an unknown (external)
*      data value. the data area in this block consists only
*      of address values and any addresses pointing into the
*      dynamic memory area must point to the start of other
*      data blocks. see also description of xnblk.
*      this type of block may be used as a file control block.
*      see sysfc,sysin,sysou,s_inp,s_oup for details.
*           +------------------------------------+
*           i                xrtyp               i
*           +------------------------------------+
*           i                xrlen               i
*           +------------------------------------+
*           /                                    /
*           /                xrptr               /
*           /                                    /
*           +------------------------------------+
{xrtyp{equ{24,0{{{pointer to dummy routine b_xrt{5565
{xrlen{equ{24,xrtyp+1{{{length of xrblk in bytes{5566
{xrptr{equ{24,xrlen+1{{{start of address pointers{5567
{xrsi_{equ{24,xrptr{{{size of standard fields in xrblk{5568
{{ejc{{{{{5569
*      s_cnv (convert) function switch constants.  the values
*      are tied to the order of the entries in the svctb table
*      and hence to the branch table in s_cnv.
{cnvst{equ{24,8{{{max standard type code for convert{5575
{cnvrt{equ{24,cnvst+1{{{convert code for reals{5579
{cnvbt{equ{24,cnvrt{{{no buffers - same as real code{5582
{cnvtt{equ{24,cnvbt+1{{{bsw code for convert{5586
*      input image length
{iniln{equ{24,1024{{{default image length for compiler{5590
{inils{equ{24,1024{{{image length if -sequ in effect{5591
{ionmb{equ{24,2{{{name base used for iochn in sysio{5593
{ionmo{equ{24,4{{{name offset used for iochn in sysio{5594
*      minimum value for keyword maxlngth
*      should be larger than iniln
{mnlen{equ{24,1024{{{min value allowed keyword maxlngth{5599
{mxern{equ{24,329{{{err num inadequate startup memory{5600
*      in general, meaningful mnemonics should be used for
*      offsets. however for small integers used often in
*      literals the following general definitions are provided.
{num01{equ{24,1{{{{5606
{num02{equ{24,2{{{{5607
{num03{equ{24,3{{{{5608
{num04{equ{24,4{{{{5609
{num05{equ{24,5{{{{5610
{num06{equ{24,6{{{{5611
{num07{equ{24,7{{{{5612
{num08{equ{24,8{{{{5613
{num09{equ{24,9{{{{5614
{num10{equ{24,10{{{{5615
{num25{equ{24,25{{{{5616
{nm320{equ{24,320{{{{5617
{nm321{equ{24,321{{{{5618
{nini8{equ{24,998{{{{5619
{nini9{equ{24,999{{{{5620
{thsnd{equ{24,1000{{{{5621
{{ejc{{{{{5622
*      numbers of undefined spitbol operators
{opbun{equ{24,5{{{no. of binary undefined ops{5626
{opuun{equ{24,6{{{no of unary undefined ops{5627
*      offsets used in prtsn, prtmi and acess
{prsnf{equ{24,13{{{offset used in prtsn{5631
{prtmf{equ{24,21{{{offset to col 21 (prtmi){5632
{rilen{equ{24,1024{{{buffer length for sysri{5633
*      codes for stages of processing
{stgic{equ{24,0{{{initial compile{5637
{stgxc{equ{24,stgic+1{{{execution compile (code){5638
{stgev{equ{24,stgxc+1{{{expression eval during execution{5639
{stgxt{equ{24,stgev+1{{{execution time{5640
{stgce{equ{24,stgxt+1{{{initial compile after end line{5641
{stgxe{equ{24,stgce+1{{{exec. compile after end line{5642
{stgnd{equ{24,stgce-stgic{{{difference in stage after end{5643
{stgee{equ{24,stgxe+1{{{eval evaluating expression{5644
{stgno{equ{24,stgee+1{{{number of codes{5645
{{ejc{{{{{5646
*      statement number pad count for listr
{stnpd{equ{24,8{{{statement no. pad count{5651
*      syntax type codes
*      these codes are returned from the scane procedure.
*      they are spaced 3 apart for the benefit of expan.
{t_uop{equ{24,0{{{unary operator{5659
{t_lpr{equ{24,t_uop+3{{{left paren{5660
{t_lbr{equ{24,t_lpr+3{{{left bracket{5661
{t_cma{equ{24,t_lbr+3{{{comma{5662
{t_fnc{equ{24,t_cma+3{{{function call{5663
{t_var{equ{24,t_fnc+3{{{variable{5664
{t_con{equ{24,t_var+3{{{constant{5665
{t_bop{equ{24,t_con+3{{{binary operator{5666
{t_rpr{equ{24,t_bop+3{{{right paren{5667
{t_rbr{equ{24,t_rpr+3{{{right bracket{5668
{t_col{equ{24,t_rbr+3{{{colon{5669
{t_smc{equ{24,t_col+3{{{semi-colon{5670
*      the following definitions are used only in the goto field
{t_fgo{equ{24,t_smc+1{{{failure goto{5674
{t_sgo{equ{24,t_fgo+1{{{success goto{5675
*      the above codes are grouped so that codes for elements
*      which can legitimately immediately precede a unary
*      operator come first to facilitate operator syntax check.
{t_uok{equ{24,t_fnc{{{last code ok before unary operator{5681
{{ejc{{{{{5682
*      definitions of values for expan jump table
{t_uo0{equ{24,t_uop+0{{{unary operator, state zero{5686
{t_uo1{equ{24,t_uop+1{{{unary operator, state one{5687
{t_uo2{equ{24,t_uop+2{{{unary operator, state two{5688
{t_lp0{equ{24,t_lpr+0{{{left paren, state zero{5689
{t_lp1{equ{24,t_lpr+1{{{left paren, state one{5690
{t_lp2{equ{24,t_lpr+2{{{left paren, state two{5691
{t_lb0{equ{24,t_lbr+0{{{left bracket, state zero{5692
{t_lb1{equ{24,t_lbr+1{{{left bracket, state one{5693
{t_lb2{equ{24,t_lbr+2{{{left bracket, state two{5694
{t_cm0{equ{24,t_cma+0{{{comma, state zero{5695
{t_cm1{equ{24,t_cma+1{{{comma, state one{5696
{t_cm2{equ{24,t_cma+2{{{comma, state two{5697
{t_fn0{equ{24,t_fnc+0{{{function call, state zero{5698
{t_fn1{equ{24,t_fnc+1{{{function call, state one{5699
{t_fn2{equ{24,t_fnc+2{{{function call, state two{5700
{t_va0{equ{24,t_var+0{{{variable, state zero{5701
{t_va1{equ{24,t_var+1{{{variable, state one{5702
{t_va2{equ{24,t_var+2{{{variable, state two{5703
{t_co0{equ{24,t_con+0{{{constant, state zero{5704
{t_co1{equ{24,t_con+1{{{constant, state one{5705
{t_co2{equ{24,t_con+2{{{constant, state two{5706
{t_bo0{equ{24,t_bop+0{{{binary operator, state zero{5707
{t_bo1{equ{24,t_bop+1{{{binary operator, state one{5708
{t_bo2{equ{24,t_bop+2{{{binary operator, state two{5709
{t_rp0{equ{24,t_rpr+0{{{right paren, state zero{5710
{t_rp1{equ{24,t_rpr+1{{{right paren, state one{5711
{t_rp2{equ{24,t_rpr+2{{{right paren, state two{5712
{t_rb0{equ{24,t_rbr+0{{{right bracket, state zero{5713
{t_rb1{equ{24,t_rbr+1{{{right bracket, state one{5714
{t_rb2{equ{24,t_rbr+2{{{right bracket, state two{5715
{t_cl0{equ{24,t_col+0{{{colon, state zero{5716
{t_cl1{equ{24,t_col+1{{{colon, state one{5717
{t_cl2{equ{24,t_col+2{{{colon, state two{5718
{t_sm0{equ{24,t_smc+0{{{semicolon, state zero{5719
{t_sm1{equ{24,t_smc+1{{{semicolon, state one{5720
{t_sm2{equ{24,t_smc+2{{{semicolon, state two{5721
{t_nes{equ{24,t_sm2+1{{{number of entries in branch table{5723
{{ejc{{{{{5724
*       definition of offsets used in control card processing
{cc_do{equ{24,0{{{-double{5732
{cc_co{equ{24,cc_do+1{{{-compare{5735
{cc_du{equ{24,cc_co+1{{{-dump{5736
{cc_cp{equ{24,cc_du+1{{{-copy{5741
{cc_ej{equ{24,cc_cp+1{{{-eject{5742
{cc_er{equ{24,cc_ej+1{{{-errors{5746
{cc_ex{equ{24,cc_er+1{{{-execute{5747
{cc_fa{equ{24,cc_ex+1{{{-fail{5748
{cc_in{equ{24,cc_fa+1{{{-include{5750
{cc_ln{equ{24,cc_in+1{{{-line{5752
{cc_li{equ{24,cc_ln+1{{{-list{5753
{cc_nr{equ{24,cc_li+1{{{-noerrors{5765
{cc_nx{equ{24,cc_nr+1{{{-noexecute{5766
{cc_nf{equ{24,cc_nx+1{{{-nofail{5767
{cc_nl{equ{24,cc_nf+1{{{-nolist{5768
{cc_no{equ{24,cc_nl+1{{{-noopt{5769
{cc_np{equ{24,cc_no+1{{{-noprint{5770
{cc_op{equ{24,cc_np+1{{{-optimise{5771
{cc_pr{equ{24,cc_op+1{{{-print{5772
{cc_si{equ{24,cc_pr+1{{{-single{5773
{cc_sp{equ{24,cc_si+1{{{-space{5774
{cc_st{equ{24,cc_sp+1{{{-stitl{5775
{cc_ti{equ{24,cc_st+1{{{-title{5776
{cc_tr{equ{24,cc_ti+1{{{-trace{5777
{cc_nc{equ{24,cc_tr+1{{{number of control cards{5778
{ccnoc{equ{24,4{{{no. of chars included in match{5779
{ccofs{equ{24,7{{{offset to start of title/subtitle{5780
{ccinm{equ{24,9{{{max depth of include file nesting{5782
{{ejc{{{{{5784
*      definitions of stack offsets used in cmpil procedure
*      see description at start of cmpil procedure for details
*      of use of these locations on the stack.
{cmstm{equ{24,0{{{tree for statement body{5791
{cmsgo{equ{24,cmstm+1{{{tree for success goto{5792
{cmfgo{equ{24,cmsgo+1{{{tree for fail goto{5793
{cmcgo{equ{24,cmfgo+1{{{conditional goto flag{5794
{cmpcd{equ{24,cmcgo+1{{{previous cdblk pointer{5795
{cmffp{equ{24,cmpcd+1{{{failure fill in flag for previous{5796
{cmffc{equ{24,cmffp+1{{{failure fill in flag for current{5797
{cmsop{equ{24,cmffc+1{{{success fill in offset for previous{5798
{cmsoc{equ{24,cmsop+1{{{success fill in offset for current{5799
{cmlbl{equ{24,cmsoc+1{{{ptr to vrblk for current label{5800
{cmtra{equ{24,cmlbl+1{{{ptr to entry cdblk{5801
{cmnen{equ{24,cmtra+1{{{count of stack entries for cmpil{5803
*      a few constants used by the profiler
{pfpd1{equ{24,8{{{pad positions ...{5808
{pfpd2{equ{24,20{{{... for profile ...{5809
{pfpd3{equ{24,32{{{... printout{5810
{pf_i2{equ{24,cfp_i+cfp_i{{{size of table entry (2 ints){5811
{{ejc{{{{{5814
*      definition of limits and adjustments that are built by
*      relcr for use by the routines that relocate pointers
*      after a save file is reloaded.  see reloc etc. for usage.
*      a block of information is built that is used in
*      relocating pointers.  there are rnsi_ instances
*      of a rssi_ word structure.  each instance corresponds
*      to one of the regions that a pointer might point into.
*      each structure takes the form:
*           +------------------------------------+
*           i    address past end of section     i
*           +------------------------------------+
*           i  adjustment from old to new adrs   i
*           +------------------------------------+
*           i    address of start of section     i
*           +------------------------------------+
*      the instances are ordered thusly:
*           +------------------------------------+
*           i           dynamic storage          i
*           +------------------------------------+
*           i           static storage           i
*           +------------------------------------+
*           i       working section globals      i
*           +------------------------------------+
*           i          constant section          i
*           +------------------------------------+
*           i            code section            i
*           +------------------------------------+
*      symbolic names for these locations as offsets from
*      the first entry are provided here.
*      definitions within a section
{rlend{equ{24,0{{{end{5854
{rladj{equ{24,rlend+1{{{adjustment{5855
{rlstr{equ{24,rladj+1{{{start{5856
{rssi_{equ{24,rlstr+1{{{size of section{5857
{rnsi_{equ{24,5{{{number of structures{5858
*      overall definitions of all structures
{rldye{equ{24,0{{{dynamic region end{5862
{rldya{equ{24,rldye+1{{{dynamic region adjustment{5863
{rldys{equ{24,rldya+1{{{dynamic region start{5864
{rlste{equ{24,rldys+1{{{static region end{5865
{rlsta{equ{24,rlste+1{{{static region adjustment{5866
{rlsts{equ{24,rlsta+1{{{static region start{5867
{rlwke{equ{24,rlsts+1{{{working section globals end{5868
{rlwka{equ{24,rlwke+1{{{working section globals adjustment{5869
{rlwks{equ{24,rlwka+1{{{working section globals start{5870
{rlcne{equ{24,rlwks+1{{{constants section end{5871
{rlcna{equ{24,rlcne+1{{{constants section adjustment{5872
{rlcns{equ{24,rlcna+1{{{constants section start{5873
{rlcde{equ{24,rlcns+1{{{code section end{5874
{rlcda{equ{24,rlcde+1{{{code section adjustment{5875
{rlcds{equ{24,rlcda+1{{{code section start{5876
{rlsi_{equ{24,rlcds+1{{{number of fields in structure{5877
{{ttl{27,s p i t b o l -- constant section{{{{5880
*      this section consists entirely of assembled constants.
*      all label names are five letters. the order is
*      approximately alphabetical, but in some cases (always
*      documented), constants must be placed in some special
*      order which must not be disturbed.
*      it must also be remembered that there is a requirement
*      for no forward references which also disturbs the
*      alphabetical order in some cases.
{{sec{{{{start of constant section{5893
*      start of constant section
{c_aaa{dac{1,0{{{first location of constant section{5897
*      free store percentage (used by alloc)
{alfsp{dac{2,e_fsp{{{free store percentage{5901
*      bit constants for general use
{bits0{dbc{1,0{{{all zero bits{5905
{bits1{dbc{1,1{{{one bit in low order position{5906
{bits2{dbc{1,2{{{bit in position 2{5907
{bits3{dbc{1,4{{{bit in position 3{5908
{bits4{dbc{1,8{{{bit in position 4{5909
{bits5{dbc{1,16{{{bit in position 5{5910
{bits6{dbc{1,32{{{bit in position 6{5911
{bits7{dbc{1,64{{{bit in position 7{5912
{bits8{dbc{1,128{{{bit in position 8{5913
{bits9{dbc{1,256{{{bit in position 9{5914
{bit10{dbc{1,512{{{bit in position 10{5915
{bit11{dbc{1,1024{{{bit in position 11{5916
{bit12{dbc{1,2048{{{bit in position 12{5917
*bitsm  dbc  cfp_m            mask for max integer
{bitsm{dbc{1,0{{{mask for max integer (value filled in at runtime){5919
*      bit constants for svblk (svbit field) tests
{btfnc{dbc{2,svfnc{{{bit to test for function{5923
{btknm{dbc{2,svknm{{{bit to test for keyword number{5924
{btlbl{dbc{2,svlbl{{{bit to test for label{5925
{btffc{dbc{2,svffc{{{bit to test for fast call{5926
{btckw{dbc{2,svckw{{{bit to test for constant keyword{5927
{btkwv{dbc{2,svkwv{{{bits to test for keword with value{5928
{btprd{dbc{2,svprd{{{bit to test for predicate function{5929
{btpre{dbc{2,svpre{{{bit to test for preevaluation{5930
{btval{dbc{2,svval{{{bit to test for value{5931
{{ejc{{{{{5932
*      list of names used for control card processing
{ccnms{dtc{27,/doub/{{{{5940
{{dtc{27,/comp/{{{{5943
{{dtc{27,/dump/{{{{5945
{{dtc{27,/copy/{{{{5947
{{dtc{27,/ejec/{{{{5949
{{dtc{27,/erro/{{{{5950
{{dtc{27,/exec/{{{{5951
{{dtc{27,/fail/{{{{5952
{{dtc{27,/incl/{{{{5954
{{dtc{27,/line/{{{{5957
{{dtc{27,/list/{{{{5959
{{dtc{27,/noer/{{{{5960
{{dtc{27,/noex/{{{{5961
{{dtc{27,/nofa/{{{{5962
{{dtc{27,/noli/{{{{5963
{{dtc{27,/noop/{{{{5964
{{dtc{27,/nopr/{{{{5965
{{dtc{27,/opti/{{{{5966
{{dtc{27,/prin/{{{{5967
{{dtc{27,/sing/{{{{5968
{{dtc{27,/spac/{{{{5969
{{dtc{27,/stit/{{{{5970
{{dtc{27,/titl/{{{{5971
{{dtc{27,/trac/{{{{5972
*      header messages for dumpr procedure (scblk format)
{dmhdk{dac{6,b_scl{{{dump of keyword values{5976
{{dac{1,22{{{{5977
{{dtc{27,/dump of keyword values/{{{{5978
{dmhdv{dac{6,b_scl{{{dump of natural variables{5980
{{dac{1,25{{{{5981
{{dtc{27,/dump of natural variables/{{{{5982
{{ejc{{{{{5983
*      message text for compilation statistics
{encm1{dac{6,b_scl{{{{5987
{{dac{1,19{{{{5989
{{dtc{27,/memory used (bytes)/{{{{5990
{encm2{dac{6,b_scl{{{{5992
{{dac{1,19{{{{5993
{{dtc{27,/memory left (bytes)/{{{{5994
{encm3{dac{6,b_scl{{{{6004
{{dac{1,11{{{{6005
{{dtc{27,/comp errors/{{{{6006
{encm4{dac{6,b_scl{{{{6008
{{dac{1,20{{{{6013
{{dtc{27,/comp time (microsec)/{{{{6014
{encm5{dac{6,b_scl{{{execution suppressed{6017
{{dac{1,20{{{{6018
{{dtc{27,/execution suppressed/{{{{6019
*      string constant for abnormal end
{endab{dac{6,b_scl{{{{6023
{{dac{1,12{{{{6024
{{dtc{27,/abnormal end/{{{{6025
{{ejc{{{{{6026
*      memory overflow during initialisation
{endmo{dac{6,b_scl{{{{6030
{endml{dac{1,15{{{{6031
{{dtc{27,/memory overflow/{{{{6032
*      string constant for message issued by l_end
{endms{dac{6,b_scl{{{{6036
{{dac{1,10{{{{6037
{{dtc{27,/normal end/{{{{6038
*      fail message for stack fail section
{endso{dac{6,b_scl{{{stack overflow in garbage collector{6042
{{dac{1,36{{{{6043
{{dtc{27,/stack overflow in garbage collection/{{{{6044
*      string constant for time up
{endtu{dac{6,b_scl{{{{6048
{{dac{1,15{{{{6049
{{dtc{27,/error - time up/{{{{6050
{{ejc{{{{{6051
*      string constant for error message (error section)
{ermms{dac{6,b_scl{{{error{6055
{{dac{1,5{{{{6056
{{dtc{27,/error/{{{{6057
{ermns{dac{6,b_scl{{{string / -- /{6059
{{dac{1,4{{{{6060
{{dtc{27,/ -- /{{{{6061
*      string constant for page numbering
{lstms{dac{6,b_scl{{{page{6065
{{dac{1,5{{{{6066
{{dtc{27,/page /{{{{6067
*      listing header message
{headr{dac{6,b_scl{{{{6071
{{dac{1,25{{{{6072
{{dtc{27,/macro spitbol version 4.0/{{{{6073
{headv{dac{6,b_scl{{{for exit() version no. check{6075
{{dac{1,5{{{{6076
{{dtc{27,/15.01/{{{{6077
*      free store percentage (used by gbcol)
{gbsdp{dac{2,e_sed{{{sediment percentage{6081
*      integer constants for general use
*      icbld optimisation uses the first three.
{int_r{dac{6,b_icl{{{{6087
{intv0{dic{16,+0{{{0{6088
{inton{dac{6,b_icl{{{{6089
{intv1{dic{16,+1{{{1{6090
{inttw{dac{6,b_icl{{{{6091
{intv2{dic{16,+2{{{2{6092
{intvt{dic{16,+10{{{10{6093
{intvh{dic{16,+100{{{100{6094
{intth{dic{16,+1000{{{1000{6095
*      table used in icbld optimisation
{intab{dac{4,int_r{{{pointer to 0{6099
{{dac{4,inton{{{pointer to 1{6100
{{dac{4,inttw{{{pointer to 2{6101
{{ejc{{{{{6102
*      special pattern nodes. the following pattern nodes
*      consist simply of a pcode pointer, see match routines
*      (p_xxx) for full details of their use and format).
{ndabb{dac{6,p_abb{{{arbno{6108
{ndabd{dac{6,p_abd{{{arbno{6109
{ndarc{dac{6,p_arc{{{arb{6110
{ndexb{dac{6,p_exb{{{expression{6111
{ndfnb{dac{6,p_fnb{{{fence(){6112
{ndfnd{dac{6,p_fnd{{{fence(){6113
{ndexc{dac{6,p_exc{{{expression{6114
{ndimb{dac{6,p_imb{{{immediate assignment{6115
{ndimd{dac{6,p_imd{{{immediate assignment{6116
{ndnth{dac{6,p_nth{{{pattern end (null pattern){6117
{ndpab{dac{6,p_pab{{{pattern assignment{6118
{ndpad{dac{6,p_pad{{{pattern assignment{6119
{nduna{dac{6,p_una{{{anchor point movement{6120
*      keyword constant pattern nodes. the following nodes are
*      used as the values of pattern keywords and the initial
*      values of the corresponding natural variables. all
*      nodes are in p0blk format and the order is tied to the
*      definitions of corresponding k_xxx symbols.
{ndabo{dac{6,p_abo{{{abort{6128
{{dac{4,ndnth{{{{6129
{ndarb{dac{6,p_arb{{{arb{6130
{{dac{4,ndnth{{{{6131
{ndbal{dac{6,p_bal{{{bal{6132
{{dac{4,ndnth{{{{6133
{ndfal{dac{6,p_fal{{{fail{6134
{{dac{4,ndnth{{{{6135
{ndfen{dac{6,p_fen{{{fence{6136
{{dac{4,ndnth{{{{6137
{ndrem{dac{6,p_rem{{{rem{6138
{{dac{4,ndnth{{{{6139
{ndsuc{dac{6,p_suc{{{succeed{6140
{{dac{4,ndnth{{{{6141
*      null string. all null values point to this string. the
*      svchs field contains a blank to provide for easy default
*      processing in trace, stoptr, lpad and rpad.
*      nullw contains 10 blanks which ensures an all blank word
*      but for very exceptional machines.
{nulls{dac{6,b_scl{{{null string value{6149
{{dac{1,0{{{sclen = 0{6150
{nullw{dtc{27,/          /{{{{6151
*      constant strings for lcase and ucase keywords
{lcase{dac{6,b_scl{{{{6157
{{dac{1,26{{{{6158
{{dtc{27,/abcdefghijklmnopqrstuvwxyz/{{{{6159
{ucase{dac{6,b_scl{{{{6161
{{dac{1,26{{{{6162
{{dtc{27,/ABCDEFGHIJKLMNOPQRSTUVWXYZ/{{{{6163
{{ejc{{{{{6165
*      operator dope vectors (see dvblk format)
{opdvc{dac{6,o_cnc{{{concatenation{6169
{{dac{2,c_cnc{{{{6170
{{dac{2,llcnc{{{{6171
{{dac{2,rrcnc{{{{6172
*      opdvs is used when scanning below the top level to
*      insure that the concatenation will not be later
*      mistaken for pattern matching
{opdvp{dac{6,o_cnc{{{concatenation - not pattern match{6178
{{dac{2,c_cnp{{{{6179
{{dac{2,llcnc{{{{6180
{{dac{2,rrcnc{{{{6181
*      note that the order of the remaining entries is tied to
*      the order of the coding in the scane procedure.
{opdvs{dac{6,o_ass{{{assignment{6186
{{dac{2,c_ass{{{{6187
{{dac{2,llass{{{{6188
{{dac{2,rrass{{{{6189
{{dac{1,6{{{unary equal{6191
{{dac{2,c_uuo{{{{6192
{{dac{2,lluno{{{{6193
{{dac{6,o_pmv{{{pattern match{6195
{{dac{2,c_pmt{{{{6196
{{dac{2,llpmt{{{{6197
{{dac{2,rrpmt{{{{6198
{{dac{6,o_int{{{interrogation{6200
{{dac{2,c_uvl{{{{6201
{{dac{2,lluno{{{{6202
{{dac{1,1{{{binary ampersand{6204
{{dac{2,c_ubo{{{{6205
{{dac{2,llamp{{{{6206
{{dac{2,rramp{{{{6207
{{dac{6,o_kwv{{{keyword reference{6209
{{dac{2,c_key{{{{6210
{{dac{2,lluno{{{{6211
{{dac{6,o_alt{{{alternation{6213
{{dac{2,c_alt{{{{6214
{{dac{2,llalt{{{{6215
{{dac{2,rralt{{{{6216
{{ejc{{{{{6217
*      operator dope vectors (continued)
{{dac{1,5{{{unary vertical bar{6221
{{dac{2,c_uuo{{{{6222
{{dac{2,lluno{{{{6223
{{dac{1,0{{{binary at{6225
{{dac{2,c_ubo{{{{6226
{{dac{2,llats{{{{6227
{{dac{2,rrats{{{{6228
{{dac{6,o_cas{{{cursor assignment{6230
{{dac{2,c_unm{{{{6231
{{dac{2,lluno{{{{6232
{{dac{1,2{{{binary number sign{6234
{{dac{2,c_ubo{{{{6235
{{dac{2,llnum{{{{6236
{{dac{2,rrnum{{{{6237
{{dac{1,7{{{unary number sign{6239
{{dac{2,c_uuo{{{{6240
{{dac{2,lluno{{{{6241
{{dac{6,o_dvd{{{division{6243
{{dac{2,c_bvl{{{{6244
{{dac{2,lldvd{{{{6245
{{dac{2,rrdvd{{{{6246
{{dac{1,9{{{unary slash{6248
{{dac{2,c_uuo{{{{6249
{{dac{2,lluno{{{{6250
{{dac{6,o_mlt{{{multiplication{6252
{{dac{2,c_bvl{{{{6253
{{dac{2,llmlt{{{{6254
{{dac{2,rrmlt{{{{6255
{{ejc{{{{{6256
*      operator dope vectors (continued)
{{dac{1,0{{{deferred expression{6260
{{dac{2,c_def{{{{6261
{{dac{2,lluno{{{{6262
{{dac{1,3{{{binary percent{6264
{{dac{2,c_ubo{{{{6265
{{dac{2,llpct{{{{6266
{{dac{2,rrpct{{{{6267
{{dac{1,8{{{unary percent{6269
{{dac{2,c_uuo{{{{6270
{{dac{2,lluno{{{{6271
{{dac{6,o_exp{{{exponentiation{6273
{{dac{2,c_bvl{{{{6274
{{dac{2,llexp{{{{6275
{{dac{2,rrexp{{{{6276
{{dac{1,10{{{unary exclamation{6278
{{dac{2,c_uuo{{{{6279
{{dac{2,lluno{{{{6280
{{dac{6,o_ima{{{immediate assignment{6282
{{dac{2,c_bvn{{{{6283
{{dac{2,lldld{{{{6284
{{dac{2,rrdld{{{{6285
{{dac{6,o_inv{{{indirection{6287
{{dac{2,c_ind{{{{6288
{{dac{2,lluno{{{{6289
{{dac{1,4{{{binary not{6291
{{dac{2,c_ubo{{{{6292
{{dac{2,llnot{{{{6293
{{dac{2,rrnot{{{{6294
{{dac{1,0{{{negation{6296
{{dac{2,c_neg{{{{6297
{{dac{2,lluno{{{{6298
{{ejc{{{{{6299
*      operator dope vectors (continued)
{{dac{6,o_sub{{{subtraction{6303
{{dac{2,c_bvl{{{{6304
{{dac{2,llplm{{{{6305
{{dac{2,rrplm{{{{6306
{{dac{6,o_com{{{complementation{6308
{{dac{2,c_uvl{{{{6309
{{dac{2,lluno{{{{6310
{{dac{6,o_add{{{addition{6312
{{dac{2,c_bvl{{{{6313
{{dac{2,llplm{{{{6314
{{dac{2,rrplm{{{{6315
{{dac{6,o_aff{{{affirmation{6317
{{dac{2,c_uvl{{{{6318
{{dac{2,lluno{{{{6319
{{dac{6,o_pas{{{pattern assignment{6321
{{dac{2,c_bvn{{{{6322
{{dac{2,lldld{{{{6323
{{dac{2,rrdld{{{{6324
{{dac{6,o_nam{{{name reference{6326
{{dac{2,c_unm{{{{6327
{{dac{2,lluno{{{{6328
*      special dvs for goto operators (see procedure scngf)
{opdvd{dac{6,o_god{{{direct goto{6332
{{dac{2,c_uvl{{{{6333
{{dac{2,lluno{{{{6334
{opdvn{dac{6,o_goc{{{complex normal goto{6336
{{dac{2,c_unm{{{{6337
{{dac{2,lluno{{{{6338
{{ejc{{{{{6339
*      operator entry address pointers, used in code
{oamn_{dac{6,o_amn{{{array ref (multi-subs by value){6343
{oamv_{dac{6,o_amv{{{array ref (multi-subs by value){6344
{oaon_{dac{6,o_aon{{{array ref (one sub by name){6345
{oaov_{dac{6,o_aov{{{array ref (one sub by value){6346
{ocer_{dac{6,o_cer{{{compilation error{6347
{ofex_{dac{6,o_fex{{{failure in expression evaluation{6348
{ofif_{dac{6,o_fif{{{failure during goto evaluation{6349
{ofnc_{dac{6,o_fnc{{{function call (more than one arg){6350
{ofne_{dac{6,o_fne{{{function name error{6351
{ofns_{dac{6,o_fns{{{function call (single argument){6352
{ogof_{dac{6,o_gof{{{set goto failure trap{6353
{oinn_{dac{6,o_inn{{{indirection by name{6354
{okwn_{dac{6,o_kwn{{{keyword reference by name{6355
{olex_{dac{6,o_lex{{{load expression by name{6356
{olpt_{dac{6,o_lpt{{{load pattern{6357
{olvn_{dac{6,o_lvn{{{load variable name{6358
{onta_{dac{6,o_nta{{{negation, first entry{6359
{ontb_{dac{6,o_ntb{{{negation, second entry{6360
{ontc_{dac{6,o_ntc{{{negation, third entry{6361
{opmn_{dac{6,o_pmn{{{pattern match by name{6362
{opms_{dac{6,o_pms{{{pattern match (statement){6363
{opop_{dac{6,o_pop{{{pop top stack item{6364
{ornm_{dac{6,o_rnm{{{return name from expression{6365
{orpl_{dac{6,o_rpl{{{pattern replacement{6366
{orvl_{dac{6,o_rvl{{{return value from expression{6367
{osla_{dac{6,o_sla{{{selection, first entry{6368
{oslb_{dac{6,o_slb{{{selection, second entry{6369
{oslc_{dac{6,o_slc{{{selection, third entry{6370
{osld_{dac{6,o_sld{{{selection, fourth entry{6371
{ostp_{dac{6,o_stp{{{stop execution{6372
{ounf_{dac{6,o_unf{{{unexpected failure{6373
{{ejc{{{{{6374
*      table of names of undefined binary operators for opsyn
{opsnb{dac{2,ch_at{{{at{6378
{{dac{2,ch_am{{{ampersand{6379
{{dac{2,ch_nm{{{number{6380
{{dac{2,ch_pc{{{percent{6381
{{dac{2,ch_nt{{{not{6382
*      table of names of undefined unary operators for opsyn
{opnsu{dac{2,ch_br{{{vertical bar{6386
{{dac{2,ch_eq{{{equal{6387
{{dac{2,ch_nm{{{number{6388
{{dac{2,ch_pc{{{percent{6389
{{dac{2,ch_sl{{{slash{6390
{{dac{2,ch_ex{{{exclamation{6391
*      address const containing profile table entry size
{pfi2a{dac{2,pf_i2{{{{6397
*      profiler message strings
{pfms1{dac{6,b_scl{{{{6401
{{dac{1,15{{{{6402
{{dtc{27,/program profile/{{{{6403
{pfms2{dac{6,b_scl{{{{6404
{{dac{1,42{{{{6405
{{dtc{27,/stmt    number of     -- execution time --/{{{{6406
{pfms3{dac{6,b_scl{{{{6407
{{dac{1,47{{{{6408
{{dtc{27,/number  executions  total(msec) per excn(mcsec)/{{{{6409
*      real constants for general use. note that the constants
*      starting at reav1 form a powers of ten table (used in
*      gtnum and gtstg)
{reav0{drc{17,+0.0{{{0.0{6419
{reap1{drc{17,+0.1{{{0.1{6422
{reap5{drc{17,+0.5{{{0.5{6423
{reav1{drc{17,+1.0{{{10**0{6425
{reavt{drc{17,+1.0e+1{{{10**1{6426
{{drc{17,+1.0e+2{{{10**2{6427
{{drc{17,+1.0e+3{{{10**3{6428
{{drc{17,+1.0e+4{{{10**4{6429
{{drc{17,+1.0e+5{{{10**5{6430
{{drc{17,+1.0e+6{{{10**6{6431
{{drc{17,+1.0e+7{{{10**7{6432
{{drc{17,+1.0e+8{{{10**8{6433
{{drc{17,+1.0e+9{{{10**9{6434
{reatt{drc{17,+1.0e+10{{{10**10{6435
{{ejc{{{{{6437
*      string constants (scblk format) for dtype procedure
{scarr{dac{6,b_scl{{{array{6441
{{dac{1,5{{{{6442
{{dtc{27,/array/{{{{6443
{sccod{dac{6,b_scl{{{code{6452
{{dac{1,4{{{{6453
{{dtc{27,/code/{{{{6454
{scexp{dac{6,b_scl{{{expression{6456
{{dac{1,10{{{{6457
{{dtc{27,/expression/{{{{6458
{scext{dac{6,b_scl{{{external{6460
{{dac{1,8{{{{6461
{{dtc{27,/external/{{{{6462
{scint{dac{6,b_scl{{{integer{6464
{{dac{1,7{{{{6465
{{dtc{27,/integer/{{{{6466
{scnam{dac{6,b_scl{{{name{6468
{{dac{1,4{{{{6469
{{dtc{27,/name/{{{{6470
{scnum{dac{6,b_scl{{{numeric{6472
{{dac{1,7{{{{6473
{{dtc{27,/numeric/{{{{6474
{scpat{dac{6,b_scl{{{pattern{6476
{{dac{1,7{{{{6477
{{dtc{27,/pattern/{{{{6478
{screa{dac{6,b_scl{{{real{6482
{{dac{1,4{{{{6483
{{dtc{27,/real/{{{{6484
{scstr{dac{6,b_scl{{{string{6487
{{dac{1,6{{{{6488
{{dtc{27,/string/{{{{6489
{sctab{dac{6,b_scl{{{table{6491
{{dac{1,5{{{{6492
{{dtc{27,/table/{{{{6493
{scfil{dac{6,b_scl{{{file (for extended load arguments){6495
{{dac{1,4{{{{6496
{{dtc{27,/file/{{{{6497
{{ejc{{{{{6499
*      string constants (scblk format) for kvrtn (see retrn)
{scfrt{dac{6,b_scl{{{freturn{6503
{{dac{1,7{{{{6504
{{dtc{27,/freturn/{{{{6505
{scnrt{dac{6,b_scl{{{nreturn{6507
{{dac{1,7{{{{6508
{{dtc{27,/nreturn/{{{{6509
{scrtn{dac{6,b_scl{{{return{6511
{{dac{1,6{{{{6512
{{dtc{27,/return/{{{{6513
*      datatype name table for dtype procedure. the order of
*      these entries is tied to the b_xxx definitions for blocks
*      note that slots for buffer and real data types are filled
*      even if these data types are conditionalized out of the
*      implementation.  this is done so that the block numbering
*      at bl_ar etc. remains constant in all versions.
{scnmt{dac{4,scarr{{{arblk     array{6523
{{dac{4,sccod{{{cdblk     code{6524
{{dac{4,scexp{{{exblk     expression{6525
{{dac{4,scint{{{icblk     integer{6526
{{dac{4,scnam{{{nmblk     name{6527
{{dac{4,scpat{{{p0blk     pattern{6528
{{dac{4,scpat{{{p1blk     pattern{6529
{{dac{4,scpat{{{p2blk     pattern{6530
{{dac{4,screa{{{rcblk     real{6535
{{dac{4,scstr{{{scblk     string{6537
{{dac{4,scexp{{{seblk     expression{6538
{{dac{4,sctab{{{tbblk     table{6539
{{dac{4,scarr{{{vcblk     array{6540
{{dac{4,scext{{{xnblk     external{6541
{{dac{4,scext{{{xrblk     external{6542
{{dac{4,nulls{{{bfblk     no buffer in this version{6544
*      string constant for real zero
{scre0{dac{6,b_scl{{{{6553
{{dac{1,2{{{{6554
{{dtc{27,/0./{{{{6555
{{ejc{{{{{6557
*      used to re-initialise kvstl
{stlim{dic{16,+2147483647{{{default statement limit{6565
*      dummy function block used for undefined functions
{stndf{dac{6,o_fun{{{ptr to undefined function err call{6573
{{dac{1,0{{{dummy fargs count for call circuit{6574
*      dummy code block used for undefined labels
{stndl{dac{6,l_und{{{code ptr points to undefined lbl{6578
*      dummy operator block used for undefined operators
{stndo{dac{6,o_oun{{{ptr to undefined operator err call{6582
{{dac{1,0{{{dummy fargs count for call circuit{6583
*      standard variable block. this block is used to initialize
*      the first seven fields of a newly constructed vrblk.
*      its format is tied to the vrblk definitions (see gtnvr).
{stnvr{dac{6,b_vrl{{{vrget{6589
{{dac{6,b_vrs{{{vrsto{6590
{{dac{4,nulls{{{vrval{6591
{{dac{6,b_vrg{{{vrtra{6592
{{dac{4,stndl{{{vrlbl{6593
{{dac{4,stndf{{{vrfnc{6594
{{dac{1,0{{{vrnxt{6595
{{ejc{{{{{6596
*      messages used in end of run processing (stopr)
{stpm1{dac{6,b_scl{{{in statement{6600
{{dac{1,12{{{{6601
{{dtc{27,/in statement/{{{{6602
{stpm2{dac{6,b_scl{{{{6604
{{dac{1,14{{{{6605
{{dtc{27,/stmts executed/{{{{6606
{stpm3{dac{6,b_scl{{{{6608
{{dac{1,20{{{{6609
{{dtc{27,/execution time msec /{{{{6610
{stpm4{dac{6,b_scl{{{in line{6613
{{dac{1,7{{{{6614
{{dtc{27,/in line/{{{{6615
{stpm5{dac{6,b_scl{{{{6618
{{dac{1,13{{{{6619
{{dtc{27,/regenerations/{{{{6620
{stpm6{dac{6,b_scl{{{in file{6623
{{dac{1,7{{{{6624
{{dtc{27,/in file/{{{{6625
{stpm7{dac{6,b_scl{{{{6628
{{dac{1,15{{{{6629
{{dtc{27,_stmt / microsec_{{{{6630
{stpm8{dac{6,b_scl{{{{6632
{{dac{1,15{{{{6633
{{dtc{27,_stmt / millisec_{{{{6634
{stpm9{dac{6,b_scl{{{{6636
{{dac{1,13{{{{6637
{{dtc{27,_stmt / second_{{{{6638
*      chars for /tu/ ending code
{strtu{dtc{27,/tu/{{{{6642
*      table used by convert function to check datatype name
*      the entries are ordered to correspond to branch table
*      in s_cnv
{svctb{dac{4,scstr{{{string{6648
{{dac{4,scint{{{integer{6649
{{dac{4,scnam{{{name{6650
{{dac{4,scpat{{{pattern{6651
{{dac{4,scarr{{{array{6652
{{dac{4,sctab{{{table{6653
{{dac{4,scexp{{{expression{6654
{{dac{4,sccod{{{code{6655
{{dac{4,scnum{{{numeric{6656
{{dac{4,screa{{{real{6659
{{dac{1,0{{{zero marks end of list{6665
{{ejc{{{{{6666
*      messages (scblk format) used by trace procedures
{tmasb{dac{6,b_scl{{{asterisks for trace statement no{6671
{{dac{1,13{{{{6672
{{dtc{27,/************ /{{{{6673
{tmbeb{dac{6,b_scl{{{blank-equal-blank{6676
{{dac{1,3{{{{6677
{{dtc{27,/ = /{{{{6678
*      dummy trblk for expression variable
{trbev{dac{6,b_trt{{{dummy trblk{6682
*      dummy trblk for keyword variable
{trbkv{dac{6,b_trt{{{dummy trblk{6686
*      dummy code block to return control to trxeq procedure
{trxdr{dac{6,o_txr{{{block points to return routine{6690
{trxdc{dac{4,trxdr{{{pointer to block{6691
{{ejc{{{{{6692
*      standard variable blocks
*      see svblk format for full details of the format. the
*      vrblks are ordered by length and within each length the
*      order is alphabetical by name of the variable.
{v_eqf{dbc{2,svfpr{{{eq{6700
{{dac{1,2{{{{6701
{{dtc{27,/eq/{{{{6702
{{dac{6,s_eqf{{{{6703
{{dac{1,2{{{{6704
{v_gef{dbc{2,svfpr{{{ge{6706
{{dac{1,2{{{{6707
{{dtc{27,/ge/{{{{6708
{{dac{6,s_gef{{{{6709
{{dac{1,2{{{{6710
{v_gtf{dbc{2,svfpr{{{gt{6712
{{dac{1,2{{{{6713
{{dtc{27,/gt/{{{{6714
{{dac{6,s_gtf{{{{6715
{{dac{1,2{{{{6716
{v_lef{dbc{2,svfpr{{{le{6718
{{dac{1,2{{{{6719
{{dtc{27,/le/{{{{6720
{{dac{6,s_lef{{{{6721
{{dac{1,2{{{{6722
{v_lnf{dbc{2,svfnp{{{ln{6725
{{dac{1,2{{{{6726
{{dtc{27,/ln/{{{{6727
{{dac{6,s_lnf{{{{6728
{{dac{1,1{{{{6729
{v_ltf{dbc{2,svfpr{{{lt{6732
{{dac{1,2{{{{6733
{{dtc{27,/lt/{{{{6734
{{dac{6,s_ltf{{{{6735
{{dac{1,2{{{{6736
{v_nef{dbc{2,svfpr{{{ne{6738
{{dac{1,2{{{{6739
{{dtc{27,/ne/{{{{6740
{{dac{6,s_nef{{{{6741
{{dac{1,2{{{{6742
{v_any{dbc{2,svfnp{{{any{6768
{{dac{1,3{{{{6769
{{dtc{27,/any/{{{{6770
{{dac{6,s_any{{{{6771
{{dac{1,1{{{{6772
{v_arb{dbc{2,svkvc{{{arb{6774
{{dac{1,3{{{{6775
{{dtc{27,/arb/{{{{6776
{{dac{2,k_arb{{{{6777
{{dac{4,ndarb{{{{6778
{{ejc{{{{{6779
*      standard variable blocks (continued)
{v_arg{dbc{2,svfnn{{{arg{6783
{{dac{1,3{{{{6784
{{dtc{27,/arg/{{{{6785
{{dac{6,s_arg{{{{6786
{{dac{1,2{{{{6787
{v_bal{dbc{2,svkvc{{{bal{6789
{{dac{1,3{{{{6790
{{dtc{27,/bal/{{{{6791
{{dac{2,k_bal{{{{6792
{{dac{4,ndbal{{{{6793
{v_cos{dbc{2,svfnp{{{cos{6796
{{dac{1,3{{{{6797
{{dtc{27,/cos/{{{{6798
{{dac{6,s_cos{{{{6799
{{dac{1,1{{{{6800
{v_end{dbc{2,svlbl{{{end{6803
{{dac{1,3{{{{6804
{{dtc{27,/end/{{{{6805
{{dac{6,l_end{{{{6806
{v_exp{dbc{2,svfnp{{{exp{6809
{{dac{1,3{{{{6810
{{dtc{27,/exp/{{{{6811
{{dac{6,s_exp{{{{6812
{{dac{1,1{{{{6813
{v_len{dbc{2,svfnp{{{len{6816
{{dac{1,3{{{{6817
{{dtc{27,/len/{{{{6818
{{dac{6,s_len{{{{6819
{{dac{1,1{{{{6820
{v_leq{dbc{2,svfpr{{{leq{6822
{{dac{1,3{{{{6823
{{dtc{27,/leq/{{{{6824
{{dac{6,s_leq{{{{6825
{{dac{1,2{{{{6826
{v_lge{dbc{2,svfpr{{{lge{6828
{{dac{1,3{{{{6829
{{dtc{27,/lge/{{{{6830
{{dac{6,s_lge{{{{6831
{{dac{1,2{{{{6832
{v_lgt{dbc{2,svfpr{{{lgt{6834
{{dac{1,3{{{{6835
{{dtc{27,/lgt/{{{{6836
{{dac{6,s_lgt{{{{6837
{{dac{1,2{{{{6838
{v_lle{dbc{2,svfpr{{{lle{6840
{{dac{1,3{{{{6841
{{dtc{27,/lle/{{{{6842
{{dac{6,s_lle{{{{6843
{{dac{1,2{{{{6844
{{ejc{{{{{6845
*      standard variable blocks (continued)
{v_llt{dbc{2,svfpr{{{llt{6849
{{dac{1,3{{{{6850
{{dtc{27,/llt/{{{{6851
{{dac{6,s_llt{{{{6852
{{dac{1,2{{{{6853
{v_lne{dbc{2,svfpr{{{lne{6855
{{dac{1,3{{{{6856
{{dtc{27,/lne/{{{{6857
{{dac{6,s_lne{{{{6858
{{dac{1,2{{{{6859
{v_pos{dbc{2,svfnp{{{pos{6861
{{dac{1,3{{{{6862
{{dtc{27,/pos/{{{{6863
{{dac{6,s_pos{{{{6864
{{dac{1,1{{{{6865
{v_rem{dbc{2,svkvc{{{rem{6867
{{dac{1,3{{{{6868
{{dtc{27,/rem/{{{{6869
{{dac{2,k_rem{{{{6870
{{dac{4,ndrem{{{{6871
{v_sin{dbc{2,svfnp{{{sin{6882
{{dac{1,3{{{{6883
{{dtc{27,/sin/{{{{6884
{{dac{6,s_sin{{{{6885
{{dac{1,1{{{{6886
{v_tab{dbc{2,svfnp{{{tab{6889
{{dac{1,3{{{{6890
{{dtc{27,/tab/{{{{6891
{{dac{6,s_tab{{{{6892
{{dac{1,1{{{{6893
{v_tan{dbc{2,svfnp{{{tan{6896
{{dac{1,3{{{{6897
{{dtc{27,/tan/{{{{6898
{{dac{6,s_tan{{{{6899
{{dac{1,1{{{{6900
{v_atn{dbc{2,svfnp{{{atan{6912
{{dac{1,4{{{{6913
{{dtc{27,/atan/{{{{6914
{{dac{6,s_atn{{{{6915
{{dac{1,1{{{{6916
{v_chr{dbc{2,svfnp{{{char{6926
{{dac{1,4{{{{6927
{{dtc{27,/char/{{{{6928
{{dac{6,s_chr{{{{6929
{{dac{1,1{{{{6930
{v_chp{dbc{2,svfnp{{{chop{6934
{{dac{1,4{{{{6935
{{dtc{27,/chop/{{{{6936
{{dac{6,s_chp{{{{6937
{{dac{1,1{{{{6938
{v_cod{dbc{2,svfnk{{{code{6940
{{dac{1,4{{{{6941
{{dtc{27,/code/{{{{6942
{{dac{2,k_cod{{{{6943
{{dac{6,s_cod{{{{6944
{{dac{1,1{{{{6945
{v_cop{dbc{2,svfnn{{{copy{6947
{{dac{1,4{{{{6948
{{dtc{27,/copy/{{{{6949
{{dac{6,s_cop{{{{6950
{{dac{1,1{{{{6951
{{ejc{{{{{6952
*      standard variable blocks (continued)
{v_dat{dbc{2,svfnn{{{data{6956
{{dac{1,4{{{{6957
{{dtc{27,/data/{{{{6958
{{dac{6,s_dat{{{{6959
{{dac{1,1{{{{6960
{v_dte{dbc{2,svfnn{{{date{6962
{{dac{1,4{{{{6963
{{dtc{27,/date/{{{{6964
{{dac{6,s_dte{{{{6965
{{dac{1,1{{{{6966
{v_dmp{dbc{2,svfnk{{{dump{6968
{{dac{1,4{{{{6969
{{dtc{27,/dump/{{{{6970
{{dac{2,k_dmp{{{{6971
{{dac{6,s_dmp{{{{6972
{{dac{1,1{{{{6973
{v_dup{dbc{2,svfnn{{{dupl{6975
{{dac{1,4{{{{6976
{{dtc{27,/dupl/{{{{6977
{{dac{6,s_dup{{{{6978
{{dac{1,2{{{{6979
{v_evl{dbc{2,svfnn{{{eval{6981
{{dac{1,4{{{{6982
{{dtc{27,/eval/{{{{6983
{{dac{6,s_evl{{{{6984
{{dac{1,1{{{{6985
{v_ext{dbc{2,svfnn{{{exit{6989
{{dac{1,4{{{{6990
{{dtc{27,/exit/{{{{6991
{{dac{6,s_ext{{{{6992
{{dac{1,2{{{{6993
{v_fal{dbc{2,svkvc{{{fail{6996
{{dac{1,4{{{{6997
{{dtc{27,/fail/{{{{6998
{{dac{2,k_fal{{{{6999
{{dac{4,ndfal{{{{7000
{v_fil{dbc{2,svknm{{{file{7003
{{dac{1,4{{{{7004
{{dtc{27,/file/{{{{7005
{{dac{2,k_fil{{{{7006
{v_hst{dbc{2,svfnn{{{host{7009
{{dac{1,4{{{{7010
{{dtc{27,/host/{{{{7011
{{dac{6,s_hst{{{{7012
{{dac{1,5{{{{7013
{{ejc{{{{{7014
*      standard variable blocks (continued)
{v_itm{dbc{2,svfnf{{{item{7018
{{dac{1,4{{{{7019
{{dtc{27,/item/{{{{7020
{{dac{6,s_itm{{{{7021
{{dac{1,999{{{{7022
{v_lin{dbc{2,svknm{{{line{7025
{{dac{1,4{{{{7026
{{dtc{27,/line/{{{{7027
{{dac{2,k_lin{{{{7028
{v_lod{dbc{2,svfnn{{{load{7033
{{dac{1,4{{{{7034
{{dtc{27,/load/{{{{7035
{{dac{6,s_lod{{{{7036
{{dac{1,2{{{{7037
{v_lpd{dbc{2,svfnp{{{lpad{7040
{{dac{1,4{{{{7041
{{dtc{27,/lpad/{{{{7042
{{dac{6,s_lpd{{{{7043
{{dac{1,3{{{{7044
{v_rpd{dbc{2,svfnp{{{rpad{7046
{{dac{1,4{{{{7047
{{dtc{27,/rpad/{{{{7048
{{dac{6,s_rpd{{{{7049
{{dac{1,3{{{{7050
{v_rps{dbc{2,svfnp{{{rpos{7052
{{dac{1,4{{{{7053
{{dtc{27,/rpos/{{{{7054
{{dac{6,s_rps{{{{7055
{{dac{1,1{{{{7056
{v_rtb{dbc{2,svfnp{{{rtab{7058
{{dac{1,4{{{{7059
{{dtc{27,/rtab/{{{{7060
{{dac{6,s_rtb{{{{7061
{{dac{1,1{{{{7062
{v_si_{dbc{2,svfnp{{{size{7064
{{dac{1,4{{{{7065
{{dtc{27,/size/{{{{7066
{{dac{6,s_si_{{{{7067
{{dac{1,1{{{{7068
{v_srt{dbc{2,svfnn{{{sort{7073
{{dac{1,4{{{{7074
{{dtc{27,/sort/{{{{7075
{{dac{6,s_srt{{{{7076
{{dac{1,2{{{{7077
{v_spn{dbc{2,svfnp{{{span{7079
{{dac{1,4{{{{7080
{{dtc{27,/span/{{{{7081
{{dac{6,s_spn{{{{7082
{{dac{1,1{{{{7083
{{ejc{{{{{7084
*      standard variable blocks (continued)
{v_sqr{dbc{2,svfnp{{{sqrt{7090
{{dac{1,4{{{{7091
{{dtc{27,/sqrt/{{{{7092
{{dac{6,s_sqr{{{{7093
{{dac{1,1{{{{7094
{v_stn{dbc{2,svknm{{{stno{7096
{{dac{1,4{{{{7097
{{dtc{27,/stno/{{{{7098
{{dac{2,k_stn{{{{7099
{v_tim{dbc{2,svfnn{{{time{7101
{{dac{1,4{{{{7102
{{dtc{27,/time/{{{{7103
{{dac{6,s_tim{{{{7104
{{dac{1,0{{{{7105
{v_trm{dbc{2,svfnk{{{trim{7107
{{dac{1,4{{{{7108
{{dtc{27,/trim/{{{{7109
{{dac{2,k_trm{{{{7110
{{dac{6,s_trm{{{{7111
{{dac{1,1{{{{7112
{v_abe{dbc{2,svknm{{{abend{7114
{{dac{1,5{{{{7115
{{dtc{27,/abend/{{{{7116
{{dac{2,k_abe{{{{7117
{v_abo{dbc{2,svkvl{{{abort{7119
{{dac{1,5{{{{7120
{{dtc{27,/abort/{{{{7121
{{dac{2,k_abo{{{{7122
{{dac{6,l_abo{{{{7123
{{dac{4,ndabo{{{{7124
{v_app{dbc{2,svfnf{{{apply{7126
{{dac{1,5{{{{7127
{{dtc{27,/apply/{{{{7128
{{dac{6,s_app{{{{7129
{{dac{1,999{{{{7130
{v_abn{dbc{2,svfnp{{{arbno{7132
{{dac{1,5{{{{7133
{{dtc{27,/arbno/{{{{7134
{{dac{6,s_abn{{{{7135
{{dac{1,1{{{{7136
{v_arr{dbc{2,svfnn{{{array{7138
{{dac{1,5{{{{7139
{{dtc{27,/array/{{{{7140
{{dac{6,s_arr{{{{7141
{{dac{1,2{{{{7142
{{ejc{{{{{7143
*      standard variable blocks (continued)
{v_brk{dbc{2,svfnp{{{break{7147
{{dac{1,5{{{{7148
{{dtc{27,/break/{{{{7149
{{dac{6,s_brk{{{{7150
{{dac{1,1{{{{7151
{v_clr{dbc{2,svfnn{{{clear{7153
{{dac{1,5{{{{7154
{{dtc{27,/clear/{{{{7155
{{dac{6,s_clr{{{{7156
{{dac{1,1{{{{7157
{v_ejc{dbc{2,svfnn{{{eject{7167
{{dac{1,5{{{{7168
{{dtc{27,/eject/{{{{7169
{{dac{6,s_ejc{{{{7170
{{dac{1,1{{{{7171
{v_fen{dbc{2,svfpk{{{fence{7173
{{dac{1,5{{{{7174
{{dtc{27,/fence/{{{{7175
{{dac{2,k_fen{{{{7176
{{dac{6,s_fnc{{{{7177
{{dac{1,1{{{{7178
{{dac{4,ndfen{{{{7179
{v_fld{dbc{2,svfnn{{{field{7181
{{dac{1,5{{{{7182
{{dtc{27,/field/{{{{7183
{{dac{6,s_fld{{{{7184
{{dac{1,2{{{{7185
{v_idn{dbc{2,svfpr{{{ident{7187
{{dac{1,5{{{{7188
{{dtc{27,/ident/{{{{7189
{{dac{6,s_idn{{{{7190
{{dac{1,2{{{{7191
{v_inp{dbc{2,svfnk{{{input{7193
{{dac{1,5{{{{7194
{{dtc{27,/input/{{{{7195
{{dac{2,k_inp{{{{7196
{{dac{6,s_inp{{{{7197
{{dac{1,3{{{{7198
{v_lcs{dbc{2,svkwc{{{lcase{7201
{{dac{1,5{{{{7202
{{dtc{27,/lcase/{{{{7203
{{dac{2,k_lcs{{{{7204
{v_loc{dbc{2,svfnn{{{local{7207
{{dac{1,5{{{{7208
{{dtc{27,/local/{{{{7209
{{dac{6,s_loc{{{{7210
{{dac{1,2{{{{7211
{{ejc{{{{{7212
*      standard variable blocks (continued)
{v_ops{dbc{2,svfnn{{{opsyn{7216
{{dac{1,5{{{{7217
{{dtc{27,/opsyn/{{{{7218
{{dac{6,s_ops{{{{7219
{{dac{1,3{{{{7220
{v_rmd{dbc{2,svfnp{{{remdr{7222
{{dac{1,5{{{{7223
{{dtc{27,/remdr/{{{{7224
{{dac{6,s_rmd{{{{7225
{{dac{1,2{{{{7226
{v_rsr{dbc{2,svfnn{{{rsort{7230
{{dac{1,5{{{{7231
{{dtc{27,/rsort/{{{{7232
{{dac{6,s_rsr{{{{7233
{{dac{1,2{{{{7234
{v_tbl{dbc{2,svfnn{{{table{7237
{{dac{1,5{{{{7238
{{dtc{27,/table/{{{{7239
{{dac{6,s_tbl{{{{7240
{{dac{1,3{{{{7241
{v_tra{dbc{2,svfnk{{{trace{7243
{{dac{1,5{{{{7244
{{dtc{27,/trace/{{{{7245
{{dac{2,k_tra{{{{7246
{{dac{6,s_tra{{{{7247
{{dac{1,4{{{{7248
{v_ucs{dbc{2,svkwc{{{ucase{7251
{{dac{1,5{{{{7252
{{dtc{27,/ucase/{{{{7253
{{dac{2,k_ucs{{{{7254
{v_anc{dbc{2,svknm{{{anchor{7257
{{dac{1,6{{{{7258
{{dtc{27,/anchor/{{{{7259
{{dac{2,k_anc{{{{7260
{v_bkx{dbc{2,svfnp{{{breakx{7271
{{dac{1,6{{{{7272
{{dtc{27,/breakx/{{{{7273
{{dac{6,s_bkx{{{{7274
{{dac{1,1{{{{7275
{v_def{dbc{2,svfnn{{{define{7286
{{dac{1,6{{{{7287
{{dtc{27,/define/{{{{7288
{{dac{6,s_def{{{{7289
{{dac{1,2{{{{7290
{v_det{dbc{2,svfnn{{{detach{7292
{{dac{1,6{{{{7293
{{dtc{27,/detach/{{{{7294
{{dac{6,s_det{{{{7295
{{dac{1,1{{{{7296
{{ejc{{{{{7297
*      standard variable blocks (continued)
{v_dif{dbc{2,svfpr{{{differ{7301
{{dac{1,6{{{{7302
{{dtc{27,/differ/{{{{7303
{{dac{6,s_dif{{{{7304
{{dac{1,2{{{{7305
{v_ftr{dbc{2,svknm{{{ftrace{7307
{{dac{1,6{{{{7308
{{dtc{27,/ftrace/{{{{7309
{{dac{2,k_ftr{{{{7310
{v_lst{dbc{2,svknm{{{lastno{7321
{{dac{1,6{{{{7322
{{dtc{27,/lastno/{{{{7323
{{dac{2,k_lst{{{{7324
{v_nay{dbc{2,svfnp{{{notany{7326
{{dac{1,6{{{{7327
{{dtc{27,/notany/{{{{7328
{{dac{6,s_nay{{{{7329
{{dac{1,1{{{{7330
{v_oup{dbc{2,svfnk{{{output{7332
{{dac{1,6{{{{7333
{{dtc{27,/output/{{{{7334
{{dac{2,k_oup{{{{7335
{{dac{6,s_oup{{{{7336
{{dac{1,3{{{{7337
{v_ret{dbc{2,svlbl{{{return{7339
{{dac{1,6{{{{7340
{{dtc{27,/return/{{{{7341
{{dac{6,l_rtn{{{{7342
{v_rew{dbc{2,svfnn{{{rewind{7344
{{dac{1,6{{{{7345
{{dtc{27,/rewind/{{{{7346
{{dac{6,s_rew{{{{7347
{{dac{1,1{{{{7348
{v_stt{dbc{2,svfnn{{{stoptr{7350
{{dac{1,6{{{{7351
{{dtc{27,/stoptr/{{{{7352
{{dac{6,s_stt{{{{7353
{{dac{1,2{{{{7354
{{ejc{{{{{7355
*      standard variable blocks (continued)
{v_sub{dbc{2,svfnn{{{substr{7359
{{dac{1,6{{{{7360
{{dtc{27,/substr/{{{{7361
{{dac{6,s_sub{{{{7362
{{dac{1,3{{{{7363
{v_unl{dbc{2,svfnn{{{unload{7365
{{dac{1,6{{{{7366
{{dtc{27,/unload/{{{{7367
{{dac{6,s_unl{{{{7368
{{dac{1,1{{{{7369
{v_col{dbc{2,svfnn{{{collect{7371
{{dac{1,7{{{{7372
{{dtc{27,/collect/{{{{7373
{{dac{6,s_col{{{{7374
{{dac{1,1{{{{7375
{v_com{dbc{2,svknm{{{compare{7378
{{dac{1,7{{{{7379
{{dtc{27,/compare/{{{{7380
{{dac{2,k_com{{{{7381
{v_cnv{dbc{2,svfnn{{{convert{7384
{{dac{1,7{{{{7385
{{dtc{27,/convert/{{{{7386
{{dac{6,s_cnv{{{{7387
{{dac{1,2{{{{7388
{v_enf{dbc{2,svfnn{{{endfile{7390
{{dac{1,7{{{{7391
{{dtc{27,/endfile/{{{{7392
{{dac{6,s_enf{{{{7393
{{dac{1,1{{{{7394
{v_etx{dbc{2,svknm{{{errtext{7396
{{dac{1,7{{{{7397
{{dtc{27,/errtext/{{{{7398
{{dac{2,k_etx{{{{7399
{v_ert{dbc{2,svknm{{{errtype{7401
{{dac{1,7{{{{7402
{{dtc{27,/errtype/{{{{7403
{{dac{2,k_ert{{{{7404
{v_frt{dbc{2,svlbl{{{freturn{7406
{{dac{1,7{{{{7407
{{dtc{27,/freturn/{{{{7408
{{dac{6,l_frt{{{{7409
{v_int{dbc{2,svfpr{{{integer{7411
{{dac{1,7{{{{7412
{{dtc{27,/integer/{{{{7413
{{dac{6,s_int{{{{7414
{{dac{1,1{{{{7415
{v_nrt{dbc{2,svlbl{{{nreturn{7417
{{dac{1,7{{{{7418
{{dtc{27,/nreturn/{{{{7419
{{dac{6,l_nrt{{{{7420
{{ejc{{{{{7421
*      standard variable blocks (continued)
{v_pfl{dbc{2,svknm{{{profile{7428
{{dac{1,7{{{{7429
{{dtc{27,/profile/{{{{7430
{{dac{2,k_pfl{{{{7431
{v_rpl{dbc{2,svfnp{{{replace{7434
{{dac{1,7{{{{7435
{{dtc{27,/replace/{{{{7436
{{dac{6,s_rpl{{{{7437
{{dac{1,3{{{{7438
{v_rvs{dbc{2,svfnp{{{reverse{7440
{{dac{1,7{{{{7441
{{dtc{27,/reverse/{{{{7442
{{dac{6,s_rvs{{{{7443
{{dac{1,1{{{{7444
{v_rtn{dbc{2,svknm{{{rtntype{7446
{{dac{1,7{{{{7447
{{dtc{27,/rtntype/{{{{7448
{{dac{2,k_rtn{{{{7449
{v_stx{dbc{2,svfnn{{{setexit{7451
{{dac{1,7{{{{7452
{{dtc{27,/setexit/{{{{7453
{{dac{6,s_stx{{{{7454
{{dac{1,1{{{{7455
{v_stc{dbc{2,svknm{{{stcount{7457
{{dac{1,7{{{{7458
{{dtc{27,/stcount/{{{{7459
{{dac{2,k_stc{{{{7460
{v_stl{dbc{2,svknm{{{stlimit{7462
{{dac{1,7{{{{7463
{{dtc{27,/stlimit/{{{{7464
{{dac{2,k_stl{{{{7465
{v_suc{dbc{2,svkvc{{{succeed{7467
{{dac{1,7{{{{7468
{{dtc{27,/succeed/{{{{7469
{{dac{2,k_suc{{{{7470
{{dac{4,ndsuc{{{{7471
{v_alp{dbc{2,svkwc{{{alphabet{7473
{{dac{1,8{{{{7474
{{dtc{27,/alphabet/{{{{7475
{{dac{2,k_alp{{{{7476
{v_cnt{dbc{2,svlbl{{{continue{7478
{{dac{1,8{{{{7479
{{dtc{27,/continue/{{{{7480
{{dac{6,l_cnt{{{{7481
{{ejc{{{{{7482
*      standard variable blocks (continued)
{v_dtp{dbc{2,svfnp{{{datatype{7486
{{dac{1,8{{{{7487
{{dtc{27,/datatype/{{{{7488
{{dac{6,s_dtp{{{{7489
{{dac{1,1{{{{7490
{v_erl{dbc{2,svknm{{{errlimit{7492
{{dac{1,8{{{{7493
{{dtc{27,/errlimit/{{{{7494
{{dac{2,k_erl{{{{7495
{v_fnc{dbc{2,svknm{{{fnclevel{7497
{{dac{1,8{{{{7498
{{dtc{27,/fnclevel/{{{{7499
{{dac{2,k_fnc{{{{7500
{v_fls{dbc{2,svknm{{{fullscan{7502
{{dac{1,8{{{{7503
{{dtc{27,/fullscan/{{{{7504
{{dac{2,k_fls{{{{7505
{v_lfl{dbc{2,svknm{{{lastfile{7508
{{dac{1,8{{{{7509
{{dtc{27,/lastfile/{{{{7510
{{dac{2,k_lfl{{{{7511
{v_lln{dbc{2,svknm{{{lastline{7515
{{dac{1,8{{{{7516
{{dtc{27,/lastline/{{{{7517
{{dac{2,k_lln{{{{7518
{v_mxl{dbc{2,svknm{{{maxlngth{7521
{{dac{1,8{{{{7522
{{dtc{27,/maxlngth/{{{{7523
{{dac{2,k_mxl{{{{7524
{v_ter{dbc{1,0{{{terminal{7526
{{dac{1,8{{{{7527
{{dtc{27,/terminal/{{{{7528
{{dac{1,0{{{{7529
{v_bsp{dbc{2,svfnn{{{backspace{7532
{{dac{1,9{{{{7533
{{dtc{27,/backspace/{{{{7534
{{dac{6,s_bsp{{{{7535
{{dac{1,1{{{{7536
{v_pro{dbc{2,svfnn{{{prototype{7539
{{dac{1,9{{{{7540
{{dtc{27,/prototype/{{{{7541
{{dac{6,s_pro{{{{7542
{{dac{1,1{{{{7543
{v_scn{dbc{2,svlbl{{{scontinue{7545
{{dac{1,9{{{{7546
{{dtc{27,/scontinue/{{{{7547
{{dac{6,l_scn{{{{7548
{{dbc{1,0{{{dummy entry to end list{7550
{{dac{1,10{{{length gt 9 (scontinue){7551
{{ejc{{{{{7552
*      list of svblk pointers for keywords to be dumped. the
*      list is in the order which appears on the dump output.
{vdmkw{dac{4,v_anc{{{anchor{7557
{{dac{4,v_cod{{{code{7561
{{dac{1,1{{{compare not printed{7566
{{dac{4,v_dmp{{{dump{7569
{{dac{4,v_erl{{{errlimit{7570
{{dac{4,v_etx{{{errtext{7571
{{dac{4,v_ert{{{errtype{7572
{{dac{4,v_fil{{{file{7574
{{dac{4,v_fnc{{{fnclevel{7576
{{dac{4,v_ftr{{{ftrace{7577
{{dac{4,v_fls{{{fullscan{7578
{{dac{4,v_inp{{{input{7579
{{dac{4,v_lfl{{{lastfile{7581
{{dac{4,v_lln{{{lastline{7584
{{dac{4,v_lst{{{lastno{7586
{{dac{4,v_lin{{{line{7588
{{dac{4,v_mxl{{{maxlength{7590
{{dac{4,v_oup{{{output{7591
{{dac{4,v_pfl{{{profile{7594
{{dac{4,v_rtn{{{rtntype{7596
{{dac{4,v_stc{{{stcount{7597
{{dac{4,v_stl{{{stlimit{7598
{{dac{4,v_stn{{{stno{7599
{{dac{4,v_tra{{{trace{7600
{{dac{4,v_trm{{{trim{7601
{{dac{1,0{{{end of list{7602
*      table used by gtnvr to search svblk lists
{vsrch{dac{1,0{{{dummy entry to get proper indexing{7606
{{dac{4,v_eqf{{{start of 1 char variables (none){7607
{{dac{4,v_eqf{{{start of 2 char variables{7608
{{dac{4,v_any{{{start of 3 char variables{7609
{{dac{4,v_atn{{{start of 4 char variables{7611
{{dac{4,v_abe{{{start of 5 char variables{7619
{{dac{4,v_anc{{{start of 6 char variables{7620
{{dac{4,v_col{{{start of 7 char variables{7621
{{dac{4,v_alp{{{start of 8 char variables{7622
{{dac{4,v_bsp{{{start of 9 char variables{7624
*      last location in constant section
{c_yyy{dac{1,0{{{last location in constant section{7631
{{ttl{27,s p i t b o l -- working storage section{{{{7632
*      the working storage section contains areas which are
*      changed during execution of the program. the value
*      assembled is the initial value before execution starts.
*      all these areas are fixed length areas. variable length
*      data is stored in the static or dynamic regions of the
*      allocated data areas.
*      the values in this area are described either as work
*      areas or as global values. a work area is used in an
*      ephemeral manner and the value is not saved from one
*      entry into a routine to another. a global value is a
*      less temporary location whose value is saved from one
*      call to another.
*      w_aaa marks the start of the working section whilst
*      w_yyy marks its end.  g_aaa marks the division between
*      temporary and global values.
*      global values are further subdivided to facilitate
*      processing by the garbage collector. r_aaa through
*      r_yyy are global values that may point into dynamic
*      storage and hence must be relocated after each garbage
*      collection.  they also serve as root pointers to all
*      allocated data that must be preserved.  pointers between
*      a_aaa and r_aaa may point into code, static storage,
*      or mark the limits of dynamic memory.  these pointers
*      must be adjusted when the working section is saved to a
*      file and subsequently reloaded at a different address.
*      a general part of the approach in this program is not
*      to overlap work areas between procedures even though a
*      small amount of space could be saved. such overlap is
*      considered a source of program errors and decreases the
*      information left behind after a system crash of any kind.
*      the names of these locations are labels with five letter
*      (a-y,_) names. as far as possible the order is kept
*      alphabetical by these names but in some cases there
*      are slight departures caused by other order requirements.
*      unless otherwise documented, the order of work areas
*      does not affect the execution of the spitbol program.
{{sec{{{{start of working storage section{7678
{{ejc{{{{{7679
*      this area is not cleared by initial code
{cmlab{dac{6,b_scl{{{string used to check label legality{7683
{{dac{1,2{{{{7684
{{dtc{27,/  /{{{{7685
*      label to mark start of work area
{w_aaa{dac{1,0{{{{7689
*      work areas for acess procedure
{actrm{dac{1,0{{{trim indicator{7693
*      work areas for alloc procedure
{aldyn{dac{1,0{{{amount of dynamic store{7697
{allia{dic{16,+0{{{dump ia{7698
{allsv{dac{1,0{{{save wb in alloc{7699
*      work areas for alost procedure
{alsta{dac{1,0{{{save wa in alost{7703
*      work areas for array function (s_arr)
{arcdm{dac{1,0{{{count dimensions{7707
{arnel{dic{16,+0{{{count elements{7708
{arptr{dac{1,0{{{offset ptr into arblk{7709
{arsvl{dic{16,+0{{{save integer low bound{7710
{{ejc{{{{{7711
*      work areas for arref routine
{arfsi{dic{16,+0{{{save current evolving subscript{7715
{arfxs{dac{1,0{{{save base stack pointer{7716
*      work areas for b_efc block routine
{befof{dac{1,0{{{save offset ptr into efblk{7720
*      work areas for b_pfc block routine
{bpfpf{dac{1,0{{{save pfblk pointer{7724
{bpfsv{dac{1,0{{{save old function value{7725
{bpfxt{dac{1,0{{{pointer to stacked arguments{7726
*      work area for collect function (s_col)
{clsvi{dic{16,+0{{{save integer argument{7730
*      work areas value for cncrd
{cnscc{dac{1,0{{{pointer to control card string{7734
{cnswc{dac{1,0{{{word count{7735
{cnr_t{dac{1,0{{{pointer to r_ttl or r_stl{7736
*      work areas for convert function (s_cnv)
{cnvtp{dac{1,0{{{save ptr into scvtb{7740
*      work areas for data function (s_dat)
{datdv{dac{1,0{{{save vrblk ptr for datatype name{7744
{datxs{dac{1,0{{{save initial stack pointer{7745
*      work areas for define function (s_def)
{deflb{dac{1,0{{{save vrblk ptr for label{7749
{defna{dac{1,0{{{count function arguments{7750
{defvr{dac{1,0{{{save vrblk ptr for function name{7751
{defxs{dac{1,0{{{save initial stack pointer{7752
*      work areas for dumpr procedure
{dmarg{dac{1,0{{{dump argument{7756
{dmpsa{dac{1,0{{{preserve wa over prtvl call{7757
{dmpsb{dac{1,0{{{preserve wb over syscm call{7759
{dmpsv{dac{1,0{{{general scratch save{7761
{dmvch{dac{1,0{{{chain pointer for variable blocks{7762
{dmpch{dac{1,0{{{save sorted vrblk chain pointer{7763
{dmpkb{dac{1,0{{{dummy kvblk for use in dumpr{7764
{dmpkt{dac{1,0{{{kvvar trblk ptr (must follow dmpkb){7765
{dmpkn{dac{1,0{{{keyword number (must follow dmpkt){7766
*      work area for dtach
{dtcnb{dac{1,0{{{name base{7770
{dtcnm{dac{1,0{{{name ptr{7771
*      work areas for dupl function (s_dup)
{dupsi{dic{16,+0{{{store integer string length{7775
*      work area for endfile (s_enf)
{enfch{dac{1,0{{{for iochn chain head{7779
{{ejc{{{{{7780
*      work areas for ertex
{ertwa{dac{1,0{{{save wa{7784
{ertwb{dac{1,0{{{save wb{7785
*      work areas for evali
{evlin{dac{1,0{{{dummy pattern block pcode{7789
{evlis{dac{1,0{{{then node (must follow evlin){7790
{evliv{dac{1,0{{{value of parm1 (must follow evlis){7791
{evlio{dac{1,0{{{ptr to original node{7792
{evlif{dac{1,0{{{flag for simple/complex argument{7793
*      work area for expan
{expsv{dac{1,0{{{save op dope vector pointer{7797
*      work areas for gbcol procedure
{gbcfl{dac{1,0{{{garbage collector active flag{7801
{gbclm{dac{1,0{{{pointer to last move block (pass 3){7802
{gbcnm{dac{1,0{{{dummy first move block{7803
{gbcns{dac{1,0{{{rest of dummy block (follows gbcnm){7804
{gbcia{dic{16,+0{{{dump ia{7810
{gbcsd{dac{1,0{{{first address beyond sediment{7811
{gbcsf{dac{1,0{{{free space within sediment{7812
{gbsva{dac{1,0{{{save wa{7814
{gbsvb{dac{1,0{{{save wb{7815
{gbsvc{dac{1,0{{{save wc{7816
*      work areas for gtnvr procedure
{gnvhe{dac{1,0{{{ptr to end of hash chain{7820
{gnvnw{dac{1,0{{{number of words in string name{7821
{gnvsa{dac{1,0{{{save wa{7822
{gnvsb{dac{1,0{{{save wb{7823
{gnvsp{dac{1,0{{{pointer into vsrch table{7824
{gnvst{dac{1,0{{{pointer to chars of string{7825
*      work areas for gtarr
{gtawa{dac{1,0{{{save wa{7829
*      work areas for gtint
{gtina{dac{1,0{{{save wa{7833
{gtinb{dac{1,0{{{save wb{7834
{{ejc{{{{{7835
*      work areas for gtnum procedure
{gtnnf{dac{1,0{{{zero/nonzero for result +/-{7839
{gtnsi{dic{16,+0{{{general integer save{7840
{gtndf{dac{1,0{{{0/1 for dec point so far no/yes{7843
{gtnes{dac{1,0{{{zero/nonzero exponent +/-{7844
{gtnex{dic{16,+0{{{real exponent{7845
{gtnsc{dac{1,0{{{scale (places after point){7846
{gtnsr{drc{17,+0.0{{{general real save{7847
{gtnrd{dac{1,0{{{flag for ok real number{7848
*      work areas for gtpat procedure
{gtpsb{dac{1,0{{{save wb{7853
*      work areas for gtstg procedure
{gtssf{dac{1,0{{{0/1 for result +/-{7857
{gtsvc{dac{1,0{{{save wc{7858
{gtsvb{dac{1,0{{{save wb{7859
{gtses{dac{1,0{{{char + or - for exponent +/-{7864
{gtsrs{drc{17,+0.0{{{general real save{7865
*      work areas for gtvar procedure
{gtvrc{dac{1,0{{{save wc{7871
*      work areas for ioput
{ioptt{dac{1,0{{{type of association{7886
*      work areas for load function
{lodfn{dac{1,0{{{pointer to vrblk for func name{7892
{lodna{dac{1,0{{{count number of arguments{7893
*      mxint is value of maximum positive integer. it is computed at runtime to allow
*      the compilation of spitbol on a machine with smaller word size the the target.
{mxint{dac{1,0{{{{7899
*      work area for profiler
{pfsvw{dac{1,0{{{to save a w-reg{7905
*      work areas for prtnm procedure
{prnsi{dic{16,+0{{{scratch integer loc{7910
*      work areas for prtsn procedure
{prsna{dac{1,0{{{save wa{7914
*      work areas for prtst procedure
{prsva{dac{1,0{{{save wa{7918
{prsvb{dac{1,0{{{save wb{7919
{prsvc{dac{1,0{{{save char counter{7920
*      work area for prtnl
{prtsa{dac{1,0{{{save wa{7924
{prtsb{dac{1,0{{{save wb{7925
*      work area for prtvl
{prvsi{dac{1,0{{{save idval{7929
*      work areas for pattern match routines
{psave{dac{1,0{{{temporary save for current node ptr{7933
{psavc{dac{1,0{{{save cursor in p_spn, p_str{7934
*      work area for relaj routine
{rlals{dac{1,0{{{ptr to list of bounds and adjusts{7939
*      work area for reldn routine
{rldcd{dac{1,0{{{save code adjustment{7943
{rldst{dac{1,0{{{save static adjustment{7944
{rldls{dac{1,0{{{save list pointer{7945
*      work areas for retrn routine
{rtnbp{dac{1,0{{{to save a block pointer{7950
{rtnfv{dac{1,0{{{new function value (result){7951
{rtnsv{dac{1,0{{{old function value (saved value){7952
*      work areas for substr function (s_sub)
{sbssv{dac{1,0{{{save third argument{7956
*      work areas for scan procedure
{scnsa{dac{1,0{{{save wa{7960
{scnsb{dac{1,0{{{save wb{7961
{scnsc{dac{1,0{{{save wc{7962
{scnof{dac{1,0{{{save offset{7963
{{ejc{{{{{7966
*      work area used by sorta, sortc, sortf, sorth
{srtdf{dac{1,0{{{datatype field name{7970
{srtfd{dac{1,0{{{found dfblk address{7971
{srtff{dac{1,0{{{found field name{7972
{srtfo{dac{1,0{{{offset to field name{7973
{srtnr{dac{1,0{{{number of rows{7974
{srtof{dac{1,0{{{offset within row to sort key{7975
{srtrt{dac{1,0{{{root offset{7976
{srts1{dac{1,0{{{save offset 1{7977
{srts2{dac{1,0{{{save offset 2{7978
{srtsc{dac{1,0{{{save wc{7979
{srtsf{dac{1,0{{{sort array first row offset{7980
{srtsn{dac{1,0{{{save n{7981
{srtso{dac{1,0{{{offset to a(0){7982
{srtsr{dac{1,0{{{0, non-zero for sort, rsort{7983
{srtst{dac{1,0{{{stride from one row to next{7984
{srtwc{dac{1,0{{{dump wc{7985
*      work areas for stopr routine
{stpsi{dic{16,+0{{{save value of stcount{7990
{stpti{dic{16,+0{{{save time elapsed{7991
*      work areas for tfind procedure
{tfnsi{dic{16,+0{{{number of headers{7995
*      work areas for xscan procedure
{xscrt{dac{1,0{{{save return code{7999
{xscwb{dac{1,0{{{save register wb{8000
*      start of global values in working section
{g_aaa{dac{1,0{{{{8004
*      global value for alloc procedure
{alfsf{dic{16,+0{{{factor in free store pcntage check{8008
*      global values for cmpil procedure
{cmerc{dac{1,0{{{count of initial compile errors{8012
{cmpln{dac{1,0{{{line number of first line of stmt{8013
{cmpxs{dac{1,0{{{save stack ptr in case of errors{8014
{cmpsn{dac{1,1{{{number of next statement to compile{8015
*      global values for cncrd
{cnsil{dac{1,0{{{save scnil during include process.{8020
{cnind{dac{1,0{{{current include file nest level{8021
{cnspt{dac{1,0{{{save scnpt during include process.{8022
{cnttl{dac{1,0{{{flag for -title, -stitl{8024
*      global flag for suppression of compilation statistics.
{cpsts{dac{1,0{{{suppress comp. stats if non zero{8028
*      global values for control card switches
{cswdb{dac{1,0{{{0/1 for -single/-double{8032
{cswer{dac{1,0{{{0/1 for -errors/-noerrors{8033
{cswex{dac{1,0{{{0/1 for -execute/-noexecute{8034
{cswfl{dac{1,1{{{0/1 for -nofail/-fail{8035
{cswin{dac{2,iniln{{{xxx for -inxxx{8036
{cswls{dac{1,1{{{0/1 for -nolist/-list{8037
{cswno{dac{1,0{{{0/1 for -optimise/-noopt{8038
{cswpr{dac{1,0{{{0/1 for -noprint/-print{8039
*      global location used by patst procedure
{ctmsk{dbc{1,0{{{last bit position used in r_ctp{8043
{curid{dac{1,0{{{current id value{8044
{{ejc{{{{{8045
*      global value for cdwrd procedure
{cwcof{dac{1,0{{{next word offset in current ccblk{8049
*      global locations for dynamic storage pointers
{dnams{dac{1,0{{{size of sediment in baus{8054
*      global area for error processing.
{erich{dac{1,0{{{copy error reports to int.chan if 1{8059
{erlst{dac{1,0{{{for listr when errors go to int.ch.{8060
{errft{dac{1,0{{{fatal error flag{8061
{errsp{dac{1,0{{{error suppression flag{8062
*      global flag for suppression of execution stats
{exsts{dac{1,0{{{suppress exec stats if set{8066
*      global values for exfal and return
{flprt{dac{1,0{{{location of fail offset for return{8070
{flptr{dac{1,0{{{location of failure offset on stack{8071
*      global location to count garbage collections (gbcol)
{gbsed{dic{16,+0{{{factor in sediment pcntage check{8076
{gbcnt{dac{1,0{{{count of garbage collections{8078
*      global value for gtcod and gtexp
{gtcef{dac{1,0{{{save fail ptr in case of error{8082
*      global locations for gtstg procedure
{gtsrn{drc{17,+0.0{{{rounding factor 0.5*10**-cfp_s{8090
{gtssc{drc{17,+0.0{{{scaling value 10**cfp_s{8091
{gtswk{dac{1,0{{{ptr to work area for gtstg{8094
*      global flag for header printing
{headp{dac{1,0{{{header printed flag{8098
*      global values for variable hash table
{hshnb{dic{16,+0{{{number of hash buckets{8102
*      global areas for init
{initr{dac{1,0{{{save terminal flag{8106
{{ejc{{{{{8107
*      global values for keyword values which are stored as one
*      word integers. these values must be assembled in the
*      following order (as dictated by k_xxx definition values).
{kvabe{dac{1,0{{{abend{8113
{kvanc{dac{1,1{{{anchor{8114
{kvcod{dac{1,0{{{code{8118
{kvcom{dac{1,0{{{compare{8120
{kvdmp{dac{1,0{{{dump{8122
{kverl{dac{1,0{{{errlimit{8123
{kvert{dac{1,0{{{errtype{8124
{kvftr{dac{1,0{{{ftrace{8125
{kvfls{dac{1,1{{{fullscan{8126
{kvinp{dac{1,1{{{input{8127
{kvmxl{dac{1,5000{{{maxlength{8128
{kvoup{dac{1,1{{{output{8129
{kvpfl{dac{1,0{{{profile{8132
{kvtra{dac{1,0{{{trace{8134
{kvtrm{dac{1,1{{{trim{8135
{kvfnc{dac{1,0{{{fnclevel{8136
{kvlst{dac{1,0{{{lastno{8137
{kvlln{dac{1,0{{{lastline{8139
{kvlin{dac{1,0{{{line{8140
{kvstn{dac{1,0{{{stno{8142
*      global values for other keywords
{kvalp{dac{1,0{{{alphabet{8146
{kvrtn{dac{4,nulls{{{rtntype (scblk pointer){8147
{kvstl{dic{16,+2147483647{{{stlimit{8153
{kvstc{dic{16,+2147483647{{{stcount (counts down from stlimit){8154
*      global values for listr procedure
{lstid{dac{1,0{{{include depth of current image{8164
{lstlc{dac{1,0{{{count lines on source list page{8166
{lstnp{dac{1,0{{{max number of lines on page{8167
{lstpf{dac{1,1{{{set nonzero if current image listed{8168
{lstpg{dac{1,0{{{current source list page number{8169
{lstpo{dac{1,0{{{offset to   page nnn   message{8170
{lstsn{dac{1,0{{{remember last stmnum listed{8171
*      global maximum size of spitbol objects
{mxlen{dac{1,0{{{initialised by sysmx call{8175
*      global execution control variable
{noxeq{dac{1,0{{{set non-zero to inhibit execution{8179
*      global profiler values locations
{pfdmp{dac{1,0{{{set non-0 if &profile set non-0{8185
{pffnc{dac{1,0{{{set non-0 if funct just entered{8186
{pfstm{dic{16,+0{{{to store starting time of stmt{8187
{pfetm{dic{16,+0{{{to store ending time of stmt{8188
{pfnte{dac{1,0{{{nr of table entries{8189
{pfste{dic{16,+0{{{gets int rep of table entry size{8190
{{ejc{{{{{8193
*      global values used in pattern match routines
{pmdfl{dac{1,0{{{pattern assignment flag{8197
{pmhbs{dac{1,0{{{history stack base pointer{8198
{pmssl{dac{1,0{{{length of subject string in chars{8199
*      global values for interface polling (syspl)
{polcs{dac{1,1{{{poll interval start value{8204
{polct{dac{1,1{{{poll interval counter{8205
*      global flags used for standard file listing options
{prich{dac{1,0{{{printer on interactive channel{8210
{prstd{dac{1,0{{{tested by prtpg{8211
{prsto{dac{1,0{{{standard listing option flag{8212
*      global values for print procedures
{prbuf{dac{1,0{{{ptr to print bfr in static{8216
{precl{dac{1,0{{{extended/compact listing flag{8217
{prlen{dac{1,0{{{length of print buffer in chars{8218
{prlnw{dac{1,0{{{length of print buffer in words{8219
{profs{dac{1,0{{{offset to next location in prbuf{8220
{prtef{dac{1,0{{{endfile flag{8221
{{ejc{{{{{8222
*      global area for readr
{rdcln{dac{1,0{{{current statement line number{8226
{rdnln{dac{1,0{{{next statement line number{8227
*      global amount of memory reserved for end of execution
{rsmem{dac{1,0{{{reserve memory{8231
*      global area for stmgo counters
{stmcs{dac{1,1{{{counter startup value{8235
{stmct{dac{1,1{{{counter active value{8236
*      adjustable global values
*      all the pointers in this section can point to the
*      dynamic or the static region.
*      when a save file is reloaded, these pointers must
*      be adjusted if static or dynamic memory is now
*      at a different address.  see routine reloc for
*      additional information.
*      some values cannot be move here because of adjacency
*      constraints.  they are handled specially by reloc et al.
*      these values are kvrtn,
*      values gtswk, kvalp, and prbuf are reinitialized by
*      procedure insta, and do not need to appear here.
*      values flprt, flptr, gtcef, and stbas point into the
*      stack and are explicitly adjusted by osint's restart
*      procedure.
{a_aaa{dac{1,0{{{start of adjustable values{8258
{cmpss{dac{1,0{{{save subroutine stack ptr{8259
{dnamb{dac{1,0{{{start of dynamic area{8260
{dnamp{dac{1,0{{{next available loc in dynamic area{8261
{dname{dac{1,0{{{end of available dynamic area{8262
{hshtb{dac{1,0{{{pointer to start of vrblk hash tabl{8263
{hshte{dac{1,0{{{pointer past end of vrblk hash tabl{8264
{iniss{dac{1,0{{{save subroutine stack ptr{8265
{pftbl{dac{1,0{{{gets adrs of (imag) table base{8266
{prnmv{dac{1,0{{{vrblk ptr from last name search{8267
{statb{dac{1,0{{{start of static area{8268
{state{dac{1,0{{{end of static area{8269
{stxvr{dac{4,nulls{{{vrblk pointer or null{8270
*      relocatable global values
*      all the pointers in this section can point to blocks in
*      the dynamic storage area and must be relocated by the
*      garbage collector. they are identified by r_xxx names.
{r_aaa{dac{1,0{{{start of relocatable values{8279
{r_arf{dac{1,0{{{array block pointer for arref{8280
{r_ccb{dac{1,0{{{ptr to ccblk being built (cdwrd){8281
{r_cim{dac{1,0{{{ptr to current compiler input str{8282
{r_cmp{dac{1,0{{{copy of r_cim used in cmpil{8283
{r_cni{dac{1,0{{{ptr to next compiler input string{8284
{r_cnt{dac{1,0{{{cdblk pointer for setexit continue{8285
{r_cod{dac{1,0{{{pointer to current cdblk or exblk{8286
{r_ctp{dac{1,0{{{ptr to current ctblk for patst{8287
{r_cts{dac{1,0{{{ptr to last string scanned by patst{8288
{r_ert{dac{1,0{{{trblk pointer for errtype trace{8289
{r_etx{dac{4,nulls{{{pointer to errtext string{8290
{r_exs{dac{1,0{{{= save xl in expdm{8291
{r_fcb{dac{1,0{{{fcblk chain head{8292
{r_fnc{dac{1,0{{{trblk pointer for fnclevel trace{8293
{r_gtc{dac{1,0{{{keep code ptr for gtcod,gtexp{8294
{r_ici{dac{1,0{{{saved r_cim during include process.{8296
{r_ifa{dac{1,0{{{array of file names by incl. depth{8298
{r_ifl{dac{1,0{{{array of line nums by include depth{8299
{r_ifn{dac{1,0{{{last include file name{8301
{r_inc{dac{1,0{{{table of include file names seen{8302
{r_io1{dac{1,0{{{file arg1 for ioput{8304
{r_io2{dac{1,0{{{file arg2 for ioput{8305
{r_iof{dac{1,0{{{fcblk ptr or 0{8306
{r_ion{dac{1,0{{{name base ptr{8307
{r_iop{dac{1,0{{{predecessor block ptr for ioput{8308
{r_iot{dac{1,0{{{trblk ptr for ioput{8309
{r_pms{dac{1,0{{{subject string ptr in pattern match{8314
{r_ra2{dac{1,0{{{replace second argument last time{8315
{r_ra3{dac{1,0{{{replace third argument last time{8316
{r_rpt{dac{1,0{{{ptr to ctblk replace table last usd{8317
{r_scp{dac{1,0{{{save pointer from last scane call{8318
{r_sfc{dac{4,nulls{{{current source file name{8320
{r_sfn{dac{1,0{{{ptr to source file name table{8321
{r_sxl{dac{1,0{{{preserve xl in sortc{8323
{r_sxr{dac{1,0{{{preserve xr in sorta/sortc{8324
{r_stc{dac{1,0{{{trblk pointer for stcount trace{8325
{r_stl{dac{1,0{{{source listing sub-title{8326
{r_sxc{dac{1,0{{{code (cdblk) ptr for setexit trap{8327
{r_ttl{dac{4,nulls{{{source listing title{8328
{r_xsc{dac{1,0{{{string pointer for xscan{8329
{{ejc{{{{{8330
*      the remaining pointers in this list are used to point
*      to function blocks for normally undefined operators.
{r_uba{dac{4,stndo{{{binary at{8335
{r_ubm{dac{4,stndo{{{binary ampersand{8336
{r_ubn{dac{4,stndo{{{binary number sign{8337
{r_ubp{dac{4,stndo{{{binary percent{8338
{r_ubt{dac{4,stndo{{{binary not{8339
{r_uub{dac{4,stndo{{{unary vertical bar{8340
{r_uue{dac{4,stndo{{{unary equal{8341
{r_uun{dac{4,stndo{{{unary number sign{8342
{r_uup{dac{4,stndo{{{unary percent{8343
{r_uus{dac{4,stndo{{{unary slash{8344
{r_uux{dac{4,stndo{{{unary exclamation{8345
{r_yyy{dac{1,0{{{last relocatable location{8346
*      global locations used in scan procedure
{scnbl{dac{1,0{{{set non-zero if scanned past blanks{8350
{scncc{dac{1,0{{{non-zero to scan control card name{8351
{scngo{dac{1,0{{{set non-zero to scan goto field{8352
{scnil{dac{1,0{{{length of current input image{8353
{scnpt{dac{1,0{{{pointer to next location in r_cim{8354
{scnrs{dac{1,0{{{set non-zero to signal rescan{8355
{scnse{dac{1,0{{{start of current element{8356
{scntp{dac{1,0{{{save syntax type from last call{8357
*      global value for indicating stage (see error section)
{stage{dac{1,0{{{initial value = initial compile{8361
{{ejc{{{{{8362
*      global stack pointer
{stbas{dac{1,0{{{pointer past stack base{8366
*      global values for setexit function (s_stx)
{stxoc{dac{1,0{{{code pointer offset{8370
{stxof{dac{1,0{{{failure offset{8371
*      global value for time keeping
{timsx{dic{16,+0{{{time at start of execution{8375
{timup{dac{1,0{{{set when time up occurs{8376
*      global values for xscan and xscni procedures
{xsofs{dac{1,0{{{offset to current location in r_xsc{8380
*      label to mark end of working section
{w_yyy{dac{1,0{{{{8384
{{ttl{27,s p i t b o l -- minimal code{{{{8385
{{sec{{{{start of program section{8386
{s_aaa{ent{2,bl__i{{{mark start of code{8387
{{ttl{27,s p i t b o l -- relocation{{{{8389
*      relocation
*      the following section provides services to osint to
*      relocate portions of the workspace.  it is used when
*      a saved memory image must be restarted at a different
*      location.
*      relaj -- relocate a list of pointers
*      (wa)                  ptr past last pointer of list
*      (wb)                  ptr to first pointer of list
*      (xl)                  list of boundaries and adjustments
*      jsr  relaj            call to process list of pointers
*      (wb)                  destroyed
{relaj{prc{25,e{1,0{{entry point{8405
{{mov{11,-(xs){7,xr{{save xr{8406
{{mov{11,-(xs){8,wa{{save wa{8407
{{mov{3,rlals{7,xl{{save ptr to list of bounds{8408
{{mov{7,xr{8,wb{{ptr to first pointer to process{8409
*      merge here to check if done
{rlaj0{mov{7,xl{3,rlals{{restore xl{8413
{{bne{7,xr{9,(xs){6,rlaj1{proceed if more to do{8414
{{mov{8,wa{10,(xs)+{{restore wa{8415
{{mov{7,xr{10,(xs)+{{restore xr{8416
{{exi{{{{return to caller{8417
*      merge here to process next pointer on list
{rlaj1{mov{8,wa{9,(xr){{load next pointer on list{8421
{{lct{8,wb{18,=rnsi_{{number of sections of adjusters{8422
*      merge here to process next section of stack list
{rlaj2{bgt{8,wa{13,rlend(xl){6,rlaj3{ok if past end of section{8426
{{blt{8,wa{13,rlstr(xl){6,rlaj3{or if before start of section{8427
{{add{8,wa{13,rladj(xl){{within section, add adjustment{8428
{{mov{9,(xr){8,wa{{return updated ptr to memory{8429
{{brn{6,rlaj4{{{done with this pointer{8430
*      here if not within section
{rlaj3{add{7,xl{19,*rssi_{{advance to next section{8434
{{bct{8,wb{6,rlaj2{{jump if more to go{8435
*      here when finished processing one pointer
{rlaj4{ica{7,xr{{{increment to next ptr on list{8439
{{brn{6,rlaj0{{{jump to check  for completion{8440
{{enp{{{{end procedure relaj{8441
{{ejc{{{{{8442
*      relcr -- create relocation info after save file reload
*      (wa)                  original s_aaa code section adr
*      (wb)                  original c_aaa constant section adr
*      (wc)                  original g_aaa working section adr
*      (xr)                  ptr to start of static region
*      (cp)                  ptr to start of dynamic region
*      (xl)                  ptr to area to receive information
*      jsr  relcr            create relocation information
*      (wa,wb,wc,xr)         destroyed
*      a block of information is built at (xl) that is used
*      in relocating pointers.  there are rnsi_ instances
*      of a rssi_ word structure.  each instance corresponds
*      to one of the regions that a pointer might point into.
*      the layout of this structure is shown in the definitions
*      section, together with symbolic definitions of the
*      entries as offsets from xl.
{relcr{prc{25,e{1,0{{entry point{8463
{{add{7,xl{19,*rlsi_{{point past build area{8464
{{mov{11,-(xl){8,wa{{save original code address{8465
{{mov{8,wa{22,=s_aaa{{compute adjustment{8466
{{sub{8,wa{9,(xl){{as new s_aaa minus original s_aaa{8467
{{mov{11,-(xl){8,wa{{save code adjustment{8468
{{mov{8,wa{22,=s_yyy{{end of target code section{8469
{{sub{8,wa{22,=s_aaa{{length of code section{8470
{{add{8,wa{13,num01(xl){{plus original start address{8471
{{mov{11,-(xl){8,wa{{end of original code section{8472
{{mov{11,-(xl){8,wb{{save constant section address{8473
{{mov{8,wb{21,=c_aaa{{start of constants section{8474
{{mov{8,wa{21,=c_yyy{{end of constants section{8475
{{sub{8,wa{8,wb{{length of constants section{8476
{{sub{8,wb{9,(xl){{new c_aaa minus original c_aaa{8477
{{mov{11,-(xl){8,wb{{save constant adjustment{8478
{{add{8,wa{13,num01(xl){{length plus original start adr{8479
{{mov{11,-(xl){8,wa{{save as end of original constants{8480
{{mov{11,-(xl){8,wc{{save working globals address{8481
{{mov{8,wc{20,=g_aaa{{start of working globals section{8482
{{mov{8,wa{20,=w_yyy{{end of working section{8483
{{sub{8,wa{8,wc{{length of working globals{8484
{{sub{8,wc{9,(xl){{new g_aaa minus original g_aaa{8485
{{mov{11,-(xl){8,wc{{save working globals adjustment{8486
{{add{8,wa{13,num01(xl){{length plus original start adr{8487
{{mov{11,-(xl){8,wa{{save as end of working globals{8488
{{mov{8,wb{3,statb{{old start of static region{8489
{{mov{11,-(xl){8,wb{{save{8490
{{sub{7,xr{8,wb{{compute adjustment{8491
{{mov{11,-(xl){7,xr{{save new statb minus old statb{8492
{{mov{11,-(xl){3,state{{old end of static region{8493
{{mov{8,wb{3,dnamb{{old start of dynamic region{8494
{{mov{11,-(xl){8,wb{{save{8495
{{scp{8,wa{{{new start of dynamic{8496
{{sub{8,wa{8,wb{{compute adjustment{8497
{{mov{11,-(xl){8,wa{{save new dnamb minus old dnamb{8498
{{mov{8,wc{3,dnamp{{old end of dynamic region in use{8499
{{mov{11,-(xl){8,wc{{save as end of old dynamic region{8500
{{exi{{{{{8501
{{enp{{{{{8502
{{ejc{{{{{8503
*      reldn -- relocate pointers in the dynamic region
*      (xl)                  list of boundaries and adjustments
*      (xr)                  ptr to first location to process
*      (wc)                  ptr past last location to process
*      jsr  reldn            call to process blocks in dynamic
*      (wa,wb,wc,xr)         destroyed
*      processes all blocks in the dynamic region.  within a
*      block, pointers to the code section, constant section,
*      working globals section, static region, and dynamic
*      region are relocated as needed.
{reldn{prc{25,e{1,0{{entry point{8518
{{mov{3,rldcd{13,rlcda(xl){{save code adjustment{8519
{{mov{3,rldst{13,rlsta(xl){{save static adjustment{8520
{{mov{3,rldls{7,xl{{save list pointer{8521
*      merge here to process the next block in dynamic
{rld01{add{9,(xr){3,rldcd{{adjust block type word{8525
{{mov{7,xl{9,(xr){{load block type word{8526
{{lei{7,xl{{{load entry point id (bl_xx){8527
*      block type switch. note that blocks with no relocatable
*      fields just return to rld05 to continue to next block.
*      note that dfblks do not appear in dynamic, only in static.
*      ccblks and cmblks are not live when a save file is
*      created, and can be skipped.
*      further note:  static blocks other than vrblks discovered
*      while scanning dynamic must be adjusted at this time.
*      see processing of ffblk for example.
{{ejc{{{{{8540
*      reldn (continued)
{{bsw{7,xl{2,bl___{{switch on block type{8544
{{iff{2,bl_ar{6,rld03{{arblk{8581
{{iff{2,bl_cd{6,rld07{{cdblk{8581
{{iff{2,bl_ex{6,rld10{{exblk{8581
{{iff{2,bl_ic{6,rld05{{icblk{8581
{{iff{2,bl_nm{6,rld13{{nmblk{8581
{{iff{2,bl_p0{6,rld13{{p0blk{8581
{{iff{2,bl_p1{6,rld14{{p1blk{8581
{{iff{2,bl_p2{6,rld14{{p2blk{8581
{{iff{2,bl_rc{6,rld05{{rcblk{8581
{{iff{2,bl_sc{6,rld05{{scblk{8581
{{iff{2,bl_se{6,rld13{{seblk{8581
{{iff{2,bl_tb{6,rld17{{tbblk{8581
{{iff{2,bl_vc{6,rld17{{vcblk{8581
{{iff{2,bl_xn{6,rld05{{xnblk{8581
{{iff{2,bl_xr{6,rld20{{xrblk{8581
{{iff{2,bl_bc{6,rld05{{bcblk - dummy to fill out iffs{8581
{{iff{2,bl_pd{6,rld15{{pdblk{8581
{{iff{2,bl_tr{6,rld19{{trblk{8581
{{iff{2,bl_bf{6,rld05{{bfblk{8581
{{iff{2,bl_cc{6,rld05{{ccblk{8581
{{iff{2,bl_cm{6,rld05{{cmblk{8581
{{iff{2,bl_ct{6,rld05{{ctblk{8581
{{iff{2,bl_df{6,rld05{{dfblk{8581
{{iff{2,bl_ef{6,rld08{{efblk{8581
{{iff{2,bl_ev{6,rld09{{evblk{8581
{{iff{2,bl_ff{6,rld11{{ffblk{8581
{{iff{2,bl_kv{6,rld13{{kvblk{8581
{{iff{2,bl_pf{6,rld16{{pfblk{8581
{{iff{2,bl_te{6,rld18{{teblk{8581
{{esw{{{{end of jump table{8581
*      arblk
{rld03{mov{8,wa{13,arlen(xr){{load length{8585
{{mov{8,wb{13,arofs(xr){{set offset to 1st reloc fld (arpro){8586
*      merge here to process pointers in a block
*      (xr)                  ptr to current block
*      (wc)                  ptr past last location to process
*      (wa)                  length (reloc flds + flds at start)
*      (wb)                  offset to first reloc field
{rld04{add{8,wa{7,xr{{point past last reloc field{8595
{{add{8,wb{7,xr{{point to first reloc field{8596
{{mov{7,xl{3,rldls{{point to list of bounds{8597
{{jsr{6,relaj{{{adjust pointers{8598
{{ejc{{{{{8599
*      reldn (continued)
*      merge here to advance to next block
*      (xr)                  ptr to current block
*      (wc)                  ptr past last location to process
{rld05{mov{8,wa{9,(xr){{block type word{8609
{{jsr{6,blkln{{{get length of block{8610
{{add{7,xr{8,wa{{point to next block{8611
{{blt{7,xr{8,wc{6,rld01{continue if more to process{8612
{{mov{7,xl{3,rldls{{restore xl{8613
{{exi{{{{return to caller if done{8614
*      cdblk
{rld07{mov{8,wa{13,cdlen(xr){{load length{8627
{{mov{8,wb{19,*cdfal{{set offset{8628
{{bne{9,(xr){22,=b_cdc{6,rld04{jump back if not complex goto{8629
{{mov{8,wb{19,*cdcod{{do not process cdfal word{8630
{{brn{6,rld04{{{jump back{8631
*      efblk
*      if the efcod word points to an xnblk, the xnblk type
*      word will not be adjusted.  since this is implementation
*      dependent, we will not worry about it.
{rld08{mov{8,wa{19,*efrsl{{set length{8639
{{mov{8,wb{19,*efcod{{and offset{8640
{{brn{6,rld04{{{all set{8641
*      evblk
{rld09{mov{8,wa{19,*offs3{{point past third field{8645
{{mov{8,wb{19,*evexp{{set offset{8646
{{brn{6,rld04{{{all set{8647
*      exblk
{rld10{mov{8,wa{13,exlen(xr){{load length{8651
{{mov{8,wb{19,*exflc{{set offset{8652
{{brn{6,rld04{{{jump back{8653
{{ejc{{{{{8654
*      reldn (continued)
*      ffblk
*      this block contains a ptr to a dfblk in the static rgn.
*      because there are multiple ffblks pointing to the same
*      dfblk (one for each field name), we only process the
*      dfblk when we encounter the ffblk for the first field.
*      the dfblk in turn contains a pointer to an scblk within
*      static.
{rld11{bne{13,ffofs(xr){19,*pdfld{6,rld12{skip dfblk if not first field{8668
{{mov{11,-(xs){7,xr{{save xr{8669
{{mov{7,xr{13,ffdfp(xr){{load old ptr to dfblk{8670
{{add{7,xr{3,rldst{{current location of dfblk{8671
{{add{9,(xr){3,rldcd{{adjust dfblk type word{8672
{{mov{8,wa{13,dflen(xr){{length of dfblk{8673
{{mov{8,wb{19,*dfnam{{offset to dfnam field{8674
{{add{8,wa{7,xr{{point past last reloc field{8675
{{add{8,wb{7,xr{{point to first reloc field{8676
{{mov{7,xl{3,rldls{{point to list of bounds{8677
{{jsr{6,relaj{{{adjust pointers{8678
{{mov{7,xr{13,dfnam(xr){{pointer to static scblk{8679
{{add{9,(xr){3,rldcd{{adjust scblk type word{8680
{{mov{7,xr{10,(xs)+{{restore ffblk pointer{8681
*      ffblk (continued)
*      merge here to set up for adjustment of ptrs in ffblk
{rld12{mov{8,wa{19,*ffofs{{set length{8687
{{mov{8,wb{19,*ffdfp{{set offset{8688
{{brn{6,rld04{{{all set{8689
*      kvblk, nmblk, p0blk, seblk
{rld13{mov{8,wa{19,*offs2{{point past second field{8693
{{mov{8,wb{19,*offs1{{offset is one (only reloc fld is 2){8694
{{brn{6,rld04{{{all set{8695
*      p1blk, p2blk
*      in p2blks, parm2 contains either a bit mask or the
*      name offset of a variable.  it never requires relocation.
{rld14{mov{8,wa{19,*parm2{{length (parm2 is non-relocatable){8702
{{mov{8,wb{19,*pthen{{set offset{8703
{{brn{6,rld04{{{all set{8704
*      pdblk
*      note that the dfblk pointed to by this pdblk was
*      processed when the ffblk was encountered.  because
*      the data function will be called before any records are
*      defined, the ffblk is encountered before any
*      corresponding pdblk.
{rld15{mov{7,xl{13,pddfp(xr){{load ptr to dfblk{8714
{{add{7,xl{3,rldst{{adjust for static relocation{8715
{{mov{8,wa{13,dfpdl(xl){{get pdblk length{8716
{{mov{8,wb{19,*pddfp{{set offset{8717
{{brn{6,rld04{{{all set{8718
{{ejc{{{{{8719
*      reldn (continued)
*      pfblk
{rld16{add{13,pfvbl(xr){3,rldst{{adjust non-contiguous field{8726
{{mov{8,wa{13,pflen(xr){{get pfblk length{8727
{{mov{8,wb{19,*pfcod{{offset to first reloc{8728
{{brn{6,rld04{{{all set{8729
*      tbblk, vcblk
{rld17{mov{8,wa{13,offs2(xr){{load length{8733
{{mov{8,wb{19,*offs3{{set offset{8734
{{brn{6,rld04{{{jump back{8735
*      teblk
{rld18{mov{8,wa{19,*tesi_{{set length{8739
{{mov{8,wb{19,*tesub{{and offset{8740
{{brn{6,rld04{{{all set{8741
*      trblk
{rld19{mov{8,wa{19,*trsi_{{set length{8745
{{mov{8,wb{19,*trval{{and offset{8746
{{brn{6,rld04{{{all set{8747
*      xrblk
{rld20{mov{8,wa{13,xrlen(xr){{load length{8751
{{mov{8,wb{19,*xrptr{{set offset{8752
{{brn{6,rld04{{{jump back{8753
{{enp{{{{end procedure reldn{8754
{{ejc{{{{{8755
*      reloc -- relocate storage after save file reload
*      (xl)                  list of boundaries and adjustments
*      jsr  reloc            relocate all pointers
*      (wa,wb,wc,xr)         destroyed
*      the list of boundaries and adjustments pointed to by
*      register xl is created by a call to relcr, which should
*      be consulted for information on its structure.
{reloc{prc{25,e{1,0{{entry point{8767
{{mov{7,xr{13,rldys(xl){{old start of dynamic{8768
{{mov{8,wc{13,rldye(xl){{old end of dynamic{8769
{{add{7,xr{13,rldya(xl){{create new start of dynamic{8770
{{add{8,wc{13,rldya(xl){{create new end of dynamic{8771
{{jsr{6,reldn{{{relocate pointers in dynamic{8772
{{jsr{6,relws{{{relocate pointers in working sect{8773
{{jsr{6,relst{{{relocate pointers in static{8774
{{exi{{{{return to caller{8775
{{enp{{{{end procedure reloc{8776
{{ejc{{{{{8777
*      relst -- relocate pointers in the static region
*      (xl)                  list of boundaries and adjustments
*      jsr  relst            call to process blocks in static
*      (wa,wb,wc,xr)         destroyed
*      only vrblks on the hash chain and any profile block are
*      processed.  other static blocks (dfblks) are processed
*      during processing of dynamic blocks.
*      global work locations will be processed at this point,
*      so pointers there can be relied upon.
{relst{prc{25,e{1,0{{entry point{8792
{{mov{7,xr{3,pftbl{{profile table{8793
{{bze{7,xr{6,rls01{{branch if no table allocated{8794
{{add{9,(xr){13,rlcda(xl){{adjust block type word{8795
*      here after dealing with profiler
{rls01{mov{8,wc{3,hshtb{{point to start of hash table{8799
{{mov{8,wb{8,wc{{point to first hash bucket{8800
{{mov{8,wa{3,hshte{{point beyond hash table{8801
{{jsr{6,relaj{{{adjust bucket pointers{8802
*      loop through slots in hash table
{rls02{beq{8,wc{3,hshte{6,rls05{done if none left{8806
{{mov{7,xr{8,wc{{else copy slot pointer{8807
{{ica{8,wc{{{bump slot pointer{8808
{{sub{7,xr{19,*vrnxt{{set offset to merge into loop{8809
*      loop through vrblks on one hash chain
{rls03{mov{7,xr{13,vrnxt(xr){{point to next vrblk on chain{8813
{{bze{7,xr{6,rls02{{jump for next bucket if chain end{8814
{{mov{8,wa{19,*vrlen{{offset of first loc past ptr fields{8815
{{mov{8,wb{19,*vrget{{offset of first location in vrblk{8816
{{bnz{13,vrlen(xr){6,rls04{{jump if not system variable{8817
{{mov{8,wa{19,*vrsi_{{offset to include vrsvp field{8818
*      merge here to process fields of vrblk
{rls04{add{8,wa{7,xr{{create end ptr{8822
{{add{8,wb{7,xr{{create start ptr{8823
{{jsr{6,relaj{{{adjust pointers in vrblk{8824
{{brn{6,rls03{{{check for another vrblk on chain{8825
*      here when all vrblks processed
{rls05{exi{{{{return to caller{8829
{{enp{{{{end procedure relst{8830
{{ejc{{{{{8831
*      relws -- relocate pointers in the working section
*      (xl)                  list of boundaries and adjustments
*      jsr  relws            call to process working section
*      (wa,wb,wc,xr)         destroyed
*      pointers between a_aaa and r_yyy are examined and
*      adjusted if necessary.  the pointer kvrtn is also
*      adjusted although it lies outside this range.
*      dname is explicitly adjusted because the limits
*      on dynamic region in stack are to the area actively
*      in use (between dnamb and dnamp), and dname is outside
*      this range.
{relws{prc{25,e{1,0{{entry point{8847
{{mov{8,wb{20,=a_aaa{{point to start of adjustables{8848
{{mov{8,wa{20,=r_yyy{{point to end of adjustables{8849
{{jsr{6,relaj{{{relocate adjustable pointers{8850
{{add{3,dname{13,rldya(xl){{adjust ptr missed by relaj{8851
{{mov{8,wb{20,=kvrtn{{case of kvrtn{8852
{{mov{8,wa{8,wb{{handled specially{8853
{{ica{8,wa{{{one value to adjust{8854
{{jsr{6,relaj{{{adjust kvrtn{8855
{{exi{{{{return to caller{8856
{{enp{{{{end procedure relws{8857
{{ttl{27,s p i t b o l -- initialization{{{{8859
*      initialisation
*      the following section receives control from the system
*      at the start of a run with the registers set as follows.
*      (wa)                  initial stack pointer
*      (xr)                  points to first word of data area
*      (xl)                  points to last word of data area
{start{prc{25,e{1,0{{entry point{8869
{{mov{3,mxint{8,wb{{{8870
{{mov{4,bitsm{8,wb{{{8871
{{zer{8,wb{{{{8872
*z-
{{mov{7,xs{8,wa{{discard return{8874
{{jsr{6,systm{{{initialise timer{8875
*z+
{{sti{3,timsx{{{store time{8878
{{mov{3,statb{7,xr{{start address of static{8879
{{mov{3,rsmem{19,*e_srs{{reserve memory{8931
{{mov{3,stbas{7,xs{{store stack base{8932
{{sss{3,iniss{{{save s-r stack ptr{8933
*      now convert free store percentage to a suitable factor
*      for easy testing in alloc routine.
{{ldi{4,intvh{{{get 100{8938
{{dvi{4,alfsp{{{form 100 / alfsp{8939
{{sti{3,alfsf{{{store the factor{8940
*      now convert free sediment percentage to a suitable factor
*      for easy testing in gbcol routine.
{{ldi{4,intvh{{{get 100{8946
{{dvi{4,gbsdp{{{form 100 / gbsdp{8947
{{sti{3,gbsed{{{store the factor{8948
*      initialize values for real conversion routine
{{lct{8,wb{18,=cfp_s{{load counter for significant digits{8957
{{ldr{4,reav1{{{load 1.0{8958
*      loop to compute 10**(max number significant digits)
{ini03{mlr{4,reavt{{{* 10.0{8962
{{bct{8,wb{6,ini03{{loop till done{8963
{{str{3,gtssc{{{store 10**(max sig digits){8964
{{ldr{4,reap5{{{load 0.5{8965
{{dvr{3,gtssc{{{compute 0.5*10**(max sig digits){8966
{{str{3,gtsrn{{{store as rounding bias{8967
{{zer{8,wc{{{set to read parameters{8970
{{jsr{6,prpar{{{read them{8971
{{ejc{{{{{8972
*      now compute starting address for dynamic store and if
*      necessary request more memory.
{{sub{7,xl{19,*e_srs{{allow for reserve memory{8977
{{mov{8,wa{3,prlen{{get print buffer length{8978
{{add{8,wa{18,=cfp_a{{add no. of chars in alphabet{8979
{{add{8,wa{18,=nstmx{{add chars for gtstg bfr{8980
{{ctb{8,wa{1,8{{convert to bytes, allowing a margin{8981
{{mov{7,xr{3,statb{{point to static base{8982
{{add{7,xr{8,wa{{increment for above buffers{8983
{{add{7,xr{19,*e_hnb{{increment for hash table{8984
{{add{7,xr{19,*e_sts{{bump for initial static block{8985
{{jsr{6,sysmx{{{get mxlen{8986
{{mov{3,kvmxl{8,wa{{provisionally store as maxlngth{8987
{{mov{3,mxlen{8,wa{{and as mxlen{8988
{{bgt{7,xr{8,wa{6,ini06{skip if static hi exceeds mxlen{8989
{{ctb{8,wa{1,1{{round up and make bigger than mxlen{8990
{{mov{7,xr{8,wa{{use it instead{8991
*      here to store values which mark initial division
*      of data area into static and dynamic
{ini06{mov{3,dnamb{7,xr{{dynamic base adrs{8996
{{mov{3,dnamp{7,xr{{dynamic ptr{8997
{{bnz{8,wa{6,ini07{{skip if non-zero mxlen{8998
{{dca{7,xr{{{point a word in front{8999
{{mov{3,kvmxl{7,xr{{use as maxlngth{9000
{{mov{3,mxlen{7,xr{{and as mxlen{9001
{{ejc{{{{{9002
*      loop here if necessary till enough memory obtained
*      so that dname is above dnamb
{ini07{mov{3,dname{7,xl{{store dynamic end address{9007
{{blt{3,dnamb{7,xl{6,ini09{skip if high enough{9008
{{jsr{6,sysmm{{{request more memory{9009
{{wtb{7,xr{{{get as baus (sgd05){9010
{{add{7,xl{7,xr{{bump by amount obtained{9011
{{bnz{7,xr{6,ini07{{try again{9012
{{mov{8,wa{18,=mxern{{insufficient memory for maxlength{9014
{{zer{8,wb{{{no column number info{9015
{{zer{8,wc{{{no line number info{9016
{{mov{7,xr{18,=stgic{{initial compile stage{9017
{{mov{7,xl{21,=nulls{{no file name{9019
{{jsr{6,sysea{{{advise of error{9021
{{ppm{6,ini08{{{cant use error logic yet{9022
{{brn{6,ini08{{{force termination{9023
*      insert text for error 329 in error message table
{{erb{1,329{26,requested maxlngth too large{{{9027
{ini08{mov{7,xr{21,=endmo{{point to failure message{9029
{{mov{8,wa{4,endml{{message length{9030
{{jsr{6,syspr{{{print it (prtst not yet usable){9031
{{ppm{{{{should not fail{9032
{{zer{7,xl{{{no fcb chain yet{9033
{{mov{8,wb{18,=num10{{set special code value{9034
{{jsr{6,sysej{{{pack up (stopr not yet usable){9035
*      initialise structures at start of static region
{ini09{mov{7,xr{3,statb{{point to static again{9039
{{jsr{6,insta{{{initialize static{9040
*      initialize number of hash headers
{{mov{8,wa{18,=e_hnb{{get number of hash headers{9044
{{mti{8,wa{{{convert to integer{9045
{{sti{3,hshnb{{{store for use by gtnvr procedure{9046
{{lct{8,wa{8,wa{{counter for clearing hash table{9047
{{mov{3,hshtb{7,xr{{pointer to hash table{9048
*      loop to clear hash table
{ini11{zer{10,(xr)+{{{blank a word{9052
{{bct{8,wa{6,ini11{{loop{9053
{{mov{3,hshte{7,xr{{end of hash table adrs is kept{9054
{{mov{3,state{7,xr{{store static end address{9055
*      init table to map statement numbers to source file names
{{mov{8,wc{18,=num01{{table will have only one bucket{9060
{{mov{7,xl{21,=nulls{{default table value{9061
{{mov{3,r_sfc{7,xl{{current source file name{9062
{{jsr{6,tmake{{{create table{9063
{{mov{3,r_sfn{7,xr{{save ptr to table{9064
*      initialize table to detect duplicate include file names
{{mov{8,wc{18,=num01{{table will have only one bucket{9070
{{mov{7,xl{21,=nulls{{default table value{9071
{{jsr{6,tmake{{{create table{9072
{{mov{3,r_inc{7,xr{{save ptr to table{9073
*      initialize array to hold names of nested include files
{{mov{8,wa{18,=ccinm{{maximum nesting level{9078
{{mov{7,xl{21,=nulls{{null string default value{9079
{{jsr{6,vmake{{{create array{9080
{{ppm{{{{{9081
{{mov{3,r_ifa{7,xr{{save ptr to array{9082
*      init array to hold line numbers of nested include files
{{mov{8,wa{18,=ccinm{{maximum nesting level{9086
{{mov{7,xl{21,=inton{{integer one default value{9087
{{jsr{6,vmake{{{create array{9088
{{ppm{{{{{9089
{{mov{3,r_ifl{7,xr{{save ptr to array{9090
*z+
*      initialize variable blocks for input and output
{{mov{7,xl{21,=v_inp{{point to string /input/{9097
{{mov{8,wb{18,=trtin{{trblk type for input{9098
{{jsr{6,inout{{{perform input association{9099
{{mov{7,xl{21,=v_oup{{point to string /output/{9100
{{mov{8,wb{18,=trtou{{trblk type for output{9101
{{jsr{6,inout{{{perform output association{9102
{{mov{8,wc{3,initr{{terminal flag{9103
{{bze{8,wc{6,ini13{{skip if no terminal{9104
{{jsr{6,prpar{{{associate terminal{9105
{{ejc{{{{{9106
*      check for expiry date
{ini13{jsr{6,sysdc{{{call date check{9110
{{mov{3,flptr{7,xs{{in case stack overflows in compiler{9111
*      now compile source input code
{{jsr{6,cmpil{{{call compiler{9115
{{mov{3,r_cod{7,xr{{set ptr to first code block{9116
{{mov{3,r_ttl{21,=nulls{{forget title{9117
{{mov{3,r_stl{21,=nulls{{forget sub-title{9118
{{zer{3,r_cim{{{forget compiler input image{9119
{{zer{3,r_ccb{{{forget interim code block{9120
{{zer{3,cnind{{{in case end occurred with include{9122
{{zer{3,lstid{{{listing include depth{9123
{{zer{7,xl{{{clear dud value{9125
{{zer{8,wb{{{dont shift dynamic store up{9126
{{zer{3,dnams{{{collect sediment too{9128
{{jsr{6,gbcol{{{clear garbage left from compile{9129
{{mov{3,dnams{7,xr{{record new sediment size{9130
{{bnz{3,cpsts{6,inix0{{skip if no listing of comp stats{9134
{{jsr{6,prtpg{{{eject page{9135
*      print compile statistics
{{jsr{6,prtmm{{{print memory usage{9139
{{mti{3,cmerc{{{get count of errors as integer{9140
{{mov{7,xr{21,=encm3{{point to /compile errors/{9141
{{jsr{6,prtmi{{{print it{9142
{{mti{3,gbcnt{{{garbage collection count{9143
{{sbi{4,intv1{{{adjust for unavoidable collect{9144
{{mov{7,xr{21,=stpm5{{point to /storage regenerations/{9145
{{jsr{6,prtmi{{{print gbcol count{9146
{{jsr{6,systm{{{get time{9147
{{sbi{3,timsx{{{get compilation time{9148
{{mov{7,xr{21,=encm4{{point to compilation time (msec)/{9149
{{jsr{6,prtmi{{{print message{9150
{{add{3,lstlc{18,=num05{{bump line count{9151
{{bze{3,headp{6,inix0{{no eject if nothing printed{9153
{{jsr{6,prtpg{{{eject printer{9154
{{ejc{{{{{9156
*      prepare now to start execution
*      set default input record length
{inix0{bgt{3,cswin{18,=iniln{6,inix1{skip if not default -in72 used{9162
{{mov{3,cswin{18,=inils{{else use default record length{9163
*      reset timer
{inix1{jsr{6,systm{{{get time again{9167
{{sti{3,timsx{{{store for end run processing{9168
{{zer{3,gbcnt{{{initialise collect count{9169
{{jsr{6,sysbx{{{call before starting execution{9170
{{add{3,noxeq{3,cswex{{add -noexecute flag{9171
{{bnz{3,noxeq{6,inix2{{jump if execution suppressed{9172
*      merge when listing file set for execution.  also
*      merge here when restarting a save file or load module.
{iniy0{mnz{3,headp{{{mark headers out regardless{9182
{{zer{11,-(xs){{{set failure location on stack{9183
{{mov{3,flptr{7,xs{{save ptr to failure offset word{9184
{{mov{7,xr{3,r_cod{{load ptr to entry code block{9185
{{mov{3,stage{18,=stgxt{{set stage for execute time{9186
{{mov{3,polcs{18,=num01{{reset interface polling interval{9188
{{mov{3,polct{18,=num01{{reset interface polling interval{9189
{{mov{3,pfnte{3,cmpsn{{copy stmts compiled count in case{9193
{{mov{3,pfdmp{3,kvpfl{{start profiling if &profile set{9194
{{jsr{6,systm{{{time yet again{9195
{{sti{3,pfstm{{{{9196
{{jsr{6,stgcc{{{compute stmgo countdown counters{9198
{{bri{9,(xr){{{start xeq with first statement{9199
*      here if execution is suppressed
{inix2{zer{8,wa{{{set abend value to zero{9204
{{mov{8,wb{18,=nini9{{set special code value{9212
{{zer{7,xl{{{no fcb chain{9213
{{jsr{6,sysej{{{end of job, exit to system{9214
{{enp{{{{end procedure start{9215
*      here from osint to restart a save file or load module.
{rstrt{prc{25,e{1,0{{entry point{9219
{{mov{7,xs{3,stbas{{discard return{9220
{{zer{7,xl{{{clear xl{9221
{{brn{6,iniy0{{{resume execution{9222
{{enp{{{{end procedure rstrt{9223
{{ttl{27,s p i t b o l -- snobol4 operator routines{{{{9225
*      this section includes all routines which can be accessed
*      directly from the generated code except system functions.
*      all routines in this section start with a label of the
*      form o_xxx where xxx is three letters. the generated code
*      contains a pointer to the appropriate entry label.
*      since the general form of the generated code consists of
*      pointers to blocks whose first word is the address of the
*      actual entry point label (o_xxx).
*      these routines are in alphabetical order by their
*      entry label names (i.e. by the xxx of the o_xxx name)
*      these routines receive control as follows
*      (cp)                  pointer to next code word
*      (xs)                  current stack pointer
{{ejc{{{{{9245
*      binary plus (addition)
{o_add{ent{{{{entry point{9249
*z+
{{jsr{6,arith{{{fetch arithmetic operands{9251
{{err{1,001{26,addition left operand is not numeric{{{9252
{{err{1,002{26,addition right operand is not numeric{{{9253
{{ppm{6,oadd1{{{jump if real operands{9256
*      here to add two integers
{{adi{13,icval(xl){{{add right operand to left{9261
{{ino{6,exint{{{return integer if no overflow{9262
{{erb{1,003{26,addition caused integer overflow{{{9263
*      here to add two reals
{oadd1{adr{13,rcval(xl){{{add right operand to left{9269
{{rno{6,exrea{{{return real if no overflow{9270
{{erb{1,261{26,addition caused real overflow{{{9271
{{ejc{{{{{9273
*      unary plus (affirmation)
{o_aff{ent{{{{entry point{9277
{{mov{7,xr{10,(xs)+{{load operand{9278
{{jsr{6,gtnum{{{convert to numeric{9279
{{err{1,004{26,affirmation operand is not numeric{{{9280
{{mov{11,-(xs){7,xr{{result if converted to numeric{9281
{{lcw{7,xr{{{get next code word{9282
{{bri{9,(xr){{{execute it{9283
{{ejc{{{{{9284
*      binary bar (alternation)
{o_alt{ent{{{{entry point{9288
{{mov{7,xr{10,(xs)+{{load right operand{9289
{{jsr{6,gtpat{{{convert to pattern{9290
{{err{1,005{26,alternation right operand is not pattern{{{9291
*      merge here from special (left alternation) case
{oalt1{mov{8,wb{22,=p_alt{{set pcode for alternative node{9295
{{jsr{6,pbild{{{build alternative node{9296
{{mov{7,xl{7,xr{{save address of alternative node{9297
{{mov{7,xr{10,(xs)+{{load left operand{9298
{{jsr{6,gtpat{{{convert to pattern{9299
{{err{1,006{26,alternation left operand is not pattern{{{9300
{{beq{7,xr{22,=p_alt{6,oalt2{jump if left arg is alternation{9301
{{mov{13,pthen(xl){7,xr{{set left operand as successor{9302
{{mov{11,-(xs){7,xl{{stack result{9303
{{lcw{7,xr{{{get next code word{9304
{{bri{9,(xr){{{execute it{9305
*      come here if left argument is itself an alternation
*      the result is more efficient if we make the replacement
*      (a / b) / c = a / (b / c)
{oalt2{mov{13,pthen(xl){13,parm1(xr){{build the (b / c) node{9313
{{mov{11,-(xs){13,pthen(xr){{set a as new left arg{9314
{{mov{7,xr{7,xl{{set (b / c) as new right arg{9315
{{brn{6,oalt1{{{merge back to build a / (b / c){9316
{{ejc{{{{{9317
*      array reference (multiple subscripts, by name)
{o_amn{ent{{{{entry point{9321
{{lcw{7,xr{{{load number of subscripts{9322
{{mov{8,wb{7,xr{{set flag for by name{9323
{{brn{6,arref{{{jump to array reference routine{9324
{{ejc{{{{{9325
*      array reference (multiple subscripts, by value)
{o_amv{ent{{{{entry point{9329
{{lcw{7,xr{{{load number of subscripts{9330
{{zer{8,wb{{{set flag for by value{9331
{{brn{6,arref{{{jump to array reference routine{9332
{{ejc{{{{{9333
*      array reference (one subscript, by name)
{o_aon{ent{{{{entry point{9337
{{mov{7,xr{9,(xs){{load subscript value{9338
{{mov{7,xl{13,num01(xs){{load array value{9339
{{mov{8,wa{9,(xl){{load first word of array operand{9340
{{beq{8,wa{22,=b_vct{6,oaon2{jump if vector reference{9341
{{beq{8,wa{22,=b_tbt{6,oaon3{jump if table reference{9342
*      here to use central array reference routine
{oaon1{mov{7,xr{18,=num01{{set number of subscripts to one{9346
{{mov{8,wb{7,xr{{set flag for by name{9347
{{brn{6,arref{{{jump to array reference routine{9348
*      here if we have a vector reference
{oaon2{bne{9,(xr){22,=b_icl{6,oaon1{use long routine if not integer{9352
{{ldi{13,icval(xr){{{load integer subscript value{9353
{{mfi{8,wa{6,exfal{{copy as address int, fail if ovflo{9354
{{bze{8,wa{6,exfal{{fail if zero{9355
{{add{8,wa{18,=vcvlb{{compute offset in words{9356
{{wtb{8,wa{{{convert to bytes{9357
{{mov{9,(xs){8,wa{{complete name on stack{9358
{{blt{8,wa{13,vclen(xl){6,oaon4{exit if subscript not too large{9359
{{brn{6,exfal{{{else fail{9360
*      here for table reference
{oaon3{mnz{8,wb{{{set flag for name reference{9364
{{jsr{6,tfind{{{locate/create table element{9365
{{ppm{6,exfal{{{fail if access fails{9366
{{mov{13,num01(xs){7,xl{{store name base on stack{9367
{{mov{9,(xs){8,wa{{store name offset on stack{9368
*      here to exit with result on stack
{oaon4{lcw{7,xr{{{result on stack, get code word{9372
{{bri{9,(xr){{{execute next code word{9373
{{ejc{{{{{9374
*      array reference (one subscript, by value)
{o_aov{ent{{{{entry point{9378
{{mov{7,xr{10,(xs)+{{load subscript value{9379
{{mov{7,xl{10,(xs)+{{load array value{9380
{{mov{8,wa{9,(xl){{load first word of array operand{9381
{{beq{8,wa{22,=b_vct{6,oaov2{jump if vector reference{9382
{{beq{8,wa{22,=b_tbt{6,oaov3{jump if table reference{9383
*      here to use central array reference routine
{oaov1{mov{11,-(xs){7,xl{{restack array value{9387
{{mov{11,-(xs){7,xr{{restack subscript{9388
{{mov{7,xr{18,=num01{{set number of subscripts to one{9389
{{zer{8,wb{{{set flag for value call{9390
{{brn{6,arref{{{jump to array reference routine{9391
*      here if we have a vector reference
{oaov2{bne{9,(xr){22,=b_icl{6,oaov1{use long routine if not integer{9395
{{ldi{13,icval(xr){{{load integer subscript value{9396
{{mfi{8,wa{6,exfal{{move as one word int, fail if ovflo{9397
{{bze{8,wa{6,exfal{{fail if zero{9398
{{add{8,wa{18,=vcvlb{{compute offset in words{9399
{{wtb{8,wa{{{convert to bytes{9400
{{bge{8,wa{13,vclen(xl){6,exfal{fail if subscript too large{9401
{{jsr{6,acess{{{access value{9402
{{ppm{6,exfal{{{fail if access fails{9403
{{mov{11,-(xs){7,xr{{stack result{9404
{{lcw{7,xr{{{get next code word{9405
{{bri{9,(xr){{{execute it{9406
*      here for table reference by value
{oaov3{zer{8,wb{{{set flag for value reference{9410
{{jsr{6,tfind{{{call table search routine{9411
{{ppm{6,exfal{{{fail if access fails{9412
{{mov{11,-(xs){7,xr{{stack result{9413
{{lcw{7,xr{{{get next code word{9414
{{bri{9,(xr){{{execute it{9415
{{ejc{{{{{9416
*      assignment
{o_ass{ent{{{{entry point{9420
*      o_rpl (pattern replacement) merges here
{oass0{mov{8,wb{10,(xs)+{{load value to be assigned{9424
{{mov{8,wa{10,(xs)+{{load name offset{9425
{{mov{7,xl{9,(xs){{load name base{9426
{{mov{9,(xs){8,wb{{store assigned value as result{9427
{{jsr{6,asign{{{perform assignment{9428
{{ppm{6,exfal{{{fail if assignment fails{9429
{{lcw{7,xr{{{result on stack, get code word{9430
{{bri{9,(xr){{{execute next code word{9431
{{ejc{{{{{9432
*      compilation error
{o_cer{ent{{{{entry point{9436
{{erb{1,007{26,compilation error encountered during execution{{{9437
{{ejc{{{{{9438
*      unary at (cursor assignment)
{o_cas{ent{{{{entry point{9442
{{mov{8,wc{10,(xs)+{{load name offset (parm2){9443
{{mov{7,xr{10,(xs)+{{load name base (parm1){9444
{{mov{8,wb{22,=p_cas{{set pcode for cursor assignment{9445
{{jsr{6,pbild{{{build node{9446
{{mov{11,-(xs){7,xr{{stack result{9447
{{lcw{7,xr{{{get next code word{9448
{{bri{9,(xr){{{execute it{9449
{{ejc{{{{{9450
*      concatenation
{o_cnc{ent{{{{entry point{9454
{{mov{7,xr{9,(xs){{load right argument{9455
{{beq{7,xr{21,=nulls{6,ocnc3{jump if right arg is null{9456
{{mov{7,xl{12,1(xs){{load left argument{9457
{{beq{7,xl{21,=nulls{6,ocnc4{jump if left argument is null{9458
{{mov{8,wa{22,=b_scl{{get constant to test for string{9459
{{bne{8,wa{9,(xl){6,ocnc2{jump if left arg not a string{9460
{{bne{8,wa{9,(xr){6,ocnc2{jump if right arg not a string{9461
*      merge here to concatenate two strings
{ocnc1{mov{8,wa{13,sclen(xl){{load left argument length{9465
{{add{8,wa{13,sclen(xr){{compute result length{9466
{{jsr{6,alocs{{{allocate scblk for result{9467
{{mov{12,1(xs){7,xr{{store result ptr over left argument{9468
{{psc{7,xr{{{prepare to store chars of result{9469
{{mov{8,wa{13,sclen(xl){{get number of chars in left arg{9470
{{plc{7,xl{{{prepare to load left arg chars{9471
{{mvc{{{{move characters of left argument{9472
{{mov{7,xl{10,(xs)+{{load right arg pointer, pop stack{9473
{{mov{8,wa{13,sclen(xl){{load number of chars in right arg{9474
{{plc{7,xl{{{prepare to load right arg chars{9475
{{mvc{{{{move characters of right argument{9476
{{zer{7,xl{{{clear garbage value in xl{9477
{{lcw{7,xr{{{result on stack, get code word{9478
{{bri{9,(xr){{{execute next code word{9479
*      come here if arguments are not both strings
{ocnc2{jsr{6,gtstg{{{convert right arg to string{9483
{{ppm{6,ocnc5{{{jump if right arg is not string{9484
{{mov{7,xl{7,xr{{save right arg ptr{9485
{{jsr{6,gtstg{{{convert left arg to string{9486
{{ppm{6,ocnc6{{{jump if left arg is not a string{9487
{{mov{11,-(xs){7,xr{{stack left argument{9488
{{mov{11,-(xs){7,xl{{stack right argument{9489
{{mov{7,xl{7,xr{{move left arg to proper reg{9490
{{mov{7,xr{9,(xs){{move right arg to proper reg{9491
{{brn{6,ocnc1{{{merge back to concatenate strings{9492
{{ejc{{{{{9493
*      concatenation (continued)
*      come here for null right argument
{ocnc3{ica{7,xs{{{remove right arg from stack{9499
{{lcw{7,xr{{{left argument on stack{9500
{{bri{9,(xr){{{execute next code word{9501
*      here for null left argument
{ocnc4{ica{7,xs{{{unstack one argument{9505
{{mov{9,(xs){7,xr{{store right argument{9506
{{lcw{7,xr{{{result on stack, get code word{9507
{{bri{9,(xr){{{execute next code word{9508
*      here if right argument is not a string
{ocnc5{mov{7,xl{7,xr{{move right argument ptr{9512
{{mov{7,xr{10,(xs)+{{load left arg pointer{9513
*      merge here when left argument is not a string
{ocnc6{jsr{6,gtpat{{{convert left arg to pattern{9517
{{err{1,008{26,concatenation left operand is not a string or pattern{{{9518
{{mov{11,-(xs){7,xr{{save result on stack{9519
{{mov{7,xr{7,xl{{point to right operand{9520
{{jsr{6,gtpat{{{convert to pattern{9521
{{err{1,009{26,concatenation right operand is not a string or pattern{{{9522
{{mov{7,xl{7,xr{{move for pconc{9523
{{mov{7,xr{10,(xs)+{{reload left operand ptr{9524
{{jsr{6,pconc{{{concatenate patterns{9525
{{mov{11,-(xs){7,xr{{stack result{9526
{{lcw{7,xr{{{get next code word{9527
{{bri{9,(xr){{{execute it{9528
{{ejc{{{{{9529
*      complementation
{o_com{ent{{{{entry point{9533
{{mov{7,xr{10,(xs)+{{load operand{9534
{{mov{8,wa{9,(xr){{load type word{9535
*      merge back here after conversion
{ocom1{beq{8,wa{22,=b_icl{6,ocom2{jump if integer{9539
{{beq{8,wa{22,=b_rcl{6,ocom3{jump if real{9542
{{jsr{6,gtnum{{{else convert to numeric{9544
{{err{1,010{26,negation operand is not numeric{{{9545
{{brn{6,ocom1{{{back to check cases{9546
*      here to complement integer
{ocom2{ldi{13,icval(xr){{{load integer value{9550
{{ngi{{{{negate{9551
{{ino{6,exint{{{return integer if no overflow{9552
{{erb{1,011{26,negation caused integer overflow{{{9553
*      here to complement real
{ocom3{ldr{13,rcval(xr){{{load real value{9559
{{ngr{{{{negate{9560
{{brn{6,exrea{{{return real result{9561
{{ejc{{{{{9563
*      binary slash (division)
{o_dvd{ent{{{{entry point{9567
{{jsr{6,arith{{{fetch arithmetic operands{9568
{{err{1,012{26,division left operand is not numeric{{{9569
{{err{1,013{26,division right operand is not numeric{{{9570
{{ppm{6,odvd2{{{jump if real operands{9573
*      here to divide two integers
{{dvi{13,icval(xl){{{divide left operand by right{9578
{{ino{6,exint{{{result ok if no overflow{9579
{{erb{1,014{26,division caused integer overflow{{{9580
*      here to divide two reals
{odvd2{dvr{13,rcval(xl){{{divide left operand by right{9586
{{rno{6,exrea{{{return real if no overflow{9587
{{erb{1,262{26,division caused real overflow{{{9588
{{ejc{{{{{9590
*      exponentiation
{o_exp{ent{{{{entry point{9594
{{mov{7,xr{10,(xs)+{{load exponent{9595
{{jsr{6,gtnum{{{convert to number{9596
{{err{1,015{26,exponentiation right operand is not numeric{{{9597
{{mov{7,xl{7,xr{{move exponent to xl{9598
{{mov{7,xr{10,(xs)+{{load base{9599
{{jsr{6,gtnum{{{convert to numeric{9600
{{err{1,016{26,exponentiation left operand is not numeric{{{9601
{{beq{9,(xl){22,=b_rcl{6,oexp7{jump if real exponent{9604
{{ldi{13,icval(xl){{{load exponent{9606
{{ilt{6,oex12{{{jump if negative exponent{9607
{{beq{8,wa{22,=b_rcl{6,oexp3{jump if base is real{9610
*      here to exponentiate an integer base and integer exponent
{{mfi{8,wa{6,oexp2{{convert exponent to 1 word integer{9615
{{lct{8,wa{8,wa{{set loop counter{9616
{{ldi{13,icval(xr){{{load base as initial value{9617
{{bnz{8,wa{6,oexp1{{jump into loop if non-zero exponent{9618
{{ieq{6,oexp4{{{error if 0**0{9619
{{ldi{4,intv1{{{nonzero**0{9620
{{brn{6,exint{{{give one as result for nonzero**0{9621
*      loop to perform exponentiation
{oex13{mli{13,icval(xr){{{multiply by base{9625
{{iov{6,oexp2{{{jump if overflow{9626
{oexp1{bct{8,wa{6,oex13{{loop if more to go{9627
{{brn{6,exint{{{else return integer result{9628
*      here if integer overflow
{oexp2{erb{1,017{26,exponentiation caused integer overflow{{{9632
{{ejc{{{{{9633
*      exponentiation (continued)
*      here to exponentiate a real to an integer power
{oexp3{mfi{8,wa{6,oexp6{{convert exponent to one word{9641
{{lct{8,wa{8,wa{{set loop counter{9642
{{ldr{13,rcval(xr){{{load base as initial value{9643
{{bnz{8,wa{6,oexp5{{jump into loop if non-zero exponent{9644
{{req{6,oexp4{{{error if 0.0**0{9645
{{ldr{4,reav1{{{nonzero**0{9646
{{brn{6,exrea{{{return 1.0 if nonzero**zero{9647
*      here for error of 0**0 or 0.0**0
{oexp4{erb{1,018{26,exponentiation result is undefined{{{9652
*      loop to perform exponentiation
{oex14{mlr{13,rcval(xr){{{multiply by base{9658
{{rov{6,oexp6{{{jump if overflow{9659
{oexp5{bct{8,wa{6,oex14{{loop till computation complete{9660
{{brn{6,exrea{{{then return real result{9661
*      here if real overflow
{oexp6{erb{1,266{26,exponentiation caused real overflow{{{9665
*      here with real exponent in (xl), numeric base in (xr)
{oexp7{beq{9,(xr){22,=b_rcl{6,oexp8{jump if base real{9670
{{ldi{13,icval(xr){{{load integer base{9671
{{itr{{{{convert to real{9672
{{jsr{6,rcbld{{{create real in (xr){9673
*      here with real exponent in (xl)
*      numeric base in (xr) and ra
{oexp8{zer{8,wb{{{set positive result flag{9678
{{ldr{13,rcval(xr){{{load base to ra{9679
{{rne{6,oexp9{{{jump if base non-zero{9680
{{ldr{13,rcval(xl){{{base is zero.  check exponent{9681
{{req{6,oexp4{{{jump if 0.0 ** 0.0{9682
{{ldr{4,reav0{{{0.0 to non-zero exponent yields 0.0{9683
{{brn{6,exrea{{{return zero result{9684
*      here with non-zero base in (xr) and ra, exponent in (xl)
*      a negative base is allowed if the exponent is integral.
{oexp9{rgt{6,oex10{{{jump if base gt 0.0{9690
{{ngr{{{{make base positive{9691
{{jsr{6,rcbld{{{create positive base in (xr){9692
{{ldr{13,rcval(xl){{{examine exponent{9693
{{chp{{{{chop to integral value{9694
{{rti{6,oexp6{{{convert to integer, br if too large{9695
{{sbr{13,rcval(xl){{{chop(exponent) - exponent{9696
{{rne{6,oex11{{{non-integral power with neg base{9697
{{mfi{8,wb{{{record even/odd exponent{9698
{{anb{8,wb{4,bits1{{odd exponent yields negative result{9699
{{ldr{13,rcval(xr){{{restore base to ra{9700
*      here with positive base in ra and (xr), exponent in (xl)
{oex10{lnf{{{{log of base{9704
{{rov{6,oexp6{{{too large{9705
{{mlr{13,rcval(xl){{{times exponent{9706
{{rov{6,oexp6{{{too large{9707
{{etx{{{{e ** (exponent * ln(base)){9708
{{rov{6,oexp6{{{too large{9709
{{bze{8,wb{6,exrea{{if no sign fixup required{9710
{{ngr{{{{negative result needed{9711
{{brn{6,exrea{{{{9712
*      here for non-integral exponent with negative base
{oex11{erb{1,311{26,exponentiation of negative base to non-integral power{{{9716
*      here with negative integer exponent in ia
{oex12{mov{11,-(xs){7,xr{{stack base{9725
{{itr{{{{convert to real exponent{9726
{{jsr{6,rcbld{{{real negative exponent in (xr){9727
{{mov{7,xl{7,xr{{put exponent in xl{9728
{{mov{7,xr{10,(xs)+{{restore base value{9729
{{brn{6,oexp7{{{process real exponent{9730
{{ejc{{{{{9734
*      failure in expression evaluation
*      this entry point is used if the evaluation of an
*      expression, initiated by the evalx procedure, fails.
*      control is returned to an appropriate point in evalx.
{o_fex{ent{{{{entry point{9742
{{brn{6,evlx6{{{jump to failure loc in evalx{9743
{{ejc{{{{{9744
*      failure during evaluation of a complex or direct goto
{o_fif{ent{{{{entry point{9748
{{erb{1,020{26,goto evaluation failure{{{9749
{{ejc{{{{{9750
*      function call (more than one argument)
{o_fnc{ent{{{{entry point{9754
{{lcw{8,wa{{{load number of arguments{9755
{{lcw{7,xr{{{load function vrblk pointer{9756
{{mov{7,xl{13,vrfnc(xr){{load function pointer{9757
{{bne{8,wa{13,fargs(xl){6,cfunc{use central routine if wrong num{9758
{{bri{9,(xl){{{jump to function if arg count ok{9759
{{ejc{{{{{9760
*      function name error
{o_fne{ent{{{{entry point{9764
{{lcw{8,wa{{{get next code word{9765
{{bne{8,wa{21,=ornm_{6,ofne1{fail if not evaluating expression{9766
{{bze{13,num02(xs){6,evlx3{{ok if expr. was wanted by value{9767
*      here for error
{ofne1{erb{1,021{26,function called by name returned a value{{{9771
{{ejc{{{{{9772
*      function call (single argument)
{o_fns{ent{{{{entry point{9776
{{lcw{7,xr{{{load function vrblk pointer{9777
{{mov{8,wa{18,=num01{{set number of arguments to one{9778
{{mov{7,xl{13,vrfnc(xr){{load function pointer{9779
{{bne{8,wa{13,fargs(xl){6,cfunc{use central routine if wrong num{9780
{{bri{9,(xl){{{jump to function if arg count ok{9781
{{ejc{{{{{9782
*      call to undefined function
{o_fun{ent{{{{entry point{9785
{{erb{1,022{26,undefined function called{{{9786
{{ejc{{{{{9787
*      execute complex goto
{o_goc{ent{{{{entry point{9791
{{mov{7,xr{13,num01(xs){{load name base pointer{9792
{{bhi{7,xr{3,state{6,ogoc1{jump if not natural variable{9793
{{add{7,xr{19,*vrtra{{else point to vrtra field{9794
{{bri{9,(xr){{{and jump through it{9795
*      here if goto operand is not natural variable
{ogoc1{erb{1,023{26,goto operand is not a natural variable{{{9799
{{ejc{{{{{9800
*      execute direct goto
{o_god{ent{{{{entry point{9804
{{mov{7,xr{9,(xs){{load operand{9805
{{mov{8,wa{9,(xr){{load first word{9806
{{beq{8,wa{22,=b_cds{6,bcds0{jump if code block to code routine{9807
{{beq{8,wa{22,=b_cdc{6,bcdc0{jump if code block to code routine{9808
{{erb{1,024{26,goto operand in direct goto is not code{{{9809
{{ejc{{{{{9810
*      set goto failure trap
*      this routine is executed at the start of a complex or
*      direct failure goto to trap a subsequent fail (see exfal)
{o_gof{ent{{{{entry point{9817
{{mov{7,xr{3,flptr{{point to fail offset on stack{9818
{{ica{9,(xr){{{point failure to o_fif word{9819
{{icp{{{{point to next code word{9820
{{lcw{7,xr{{{fetch next code word{9821
{{bri{9,(xr){{{execute it{9822
{{ejc{{{{{9823
*      binary dollar (immediate assignment)
*      the pattern built by binary dollar is a compound pattern.
*      see description at start of pattern match section for
*      details of the structure which is constructed.
{o_ima{ent{{{{entry point{9831
{{mov{8,wb{22,=p_imc{{set pcode for last node{9832
{{mov{8,wc{10,(xs)+{{pop name offset (parm2){9833
{{mov{7,xr{10,(xs)+{{pop name base (parm1){9834
{{jsr{6,pbild{{{build p_imc node{9835
{{mov{7,xl{7,xr{{save ptr to node{9836
{{mov{7,xr{9,(xs){{load left argument{9837
{{jsr{6,gtpat{{{convert to pattern{9838
{{err{1,025{26,immediate assignment left operand is not pattern{{{9839
{{mov{9,(xs){7,xr{{save ptr to left operand pattern{9840
{{mov{8,wb{22,=p_ima{{set pcode for first node{9841
{{jsr{6,pbild{{{build p_ima node{9842
{{mov{13,pthen(xr){10,(xs)+{{set left operand as p_ima successor{9843
{{jsr{6,pconc{{{concatenate to form final pattern{9844
{{mov{11,-(xs){7,xr{{stack result{9845
{{lcw{7,xr{{{get next code word{9846
{{bri{9,(xr){{{execute it{9847
{{ejc{{{{{9848
*      indirection (by name)
{o_inn{ent{{{{entry point{9852
{{mnz{8,wb{{{set flag for result by name{9853
{{brn{6,indir{{{jump to common routine{9854
{{ejc{{{{{9855
*      interrogation
{o_int{ent{{{{entry point{9859
{{mov{9,(xs){21,=nulls{{replace operand with null{9860
{{lcw{7,xr{{{get next code word{9861
{{bri{9,(xr){{{execute next code word{9862
{{ejc{{{{{9863
*      indirection (by value)
{o_inv{ent{{{{entry point{9867
{{zer{8,wb{{{set flag for by value{9868
{{brn{6,indir{{{jump to common routine{9869
{{ejc{{{{{9870
*      keyword reference (by name)
{o_kwn{ent{{{{entry point{9874
{{jsr{6,kwnam{{{get keyword name{9875
{{brn{6,exnam{{{exit with result name{9876
{{ejc{{{{{9877
*      keyword reference (by value)
{o_kwv{ent{{{{entry point{9881
{{jsr{6,kwnam{{{get keyword name{9882
{{mov{3,dnamp{7,xr{{delete kvblk{9883
{{jsr{6,acess{{{access value{9884
{{ppm{6,exnul{{{dummy (unused) failure return{9885
{{mov{11,-(xs){7,xr{{stack result{9886
{{lcw{7,xr{{{get next code word{9887
{{bri{9,(xr){{{execute it{9888
{{ejc{{{{{9889
*      load expression by name
{o_lex{ent{{{{entry point{9893
{{mov{8,wa{19,*evsi_{{set size of evblk{9894
{{jsr{6,alloc{{{allocate space for evblk{9895
{{mov{9,(xr){22,=b_evt{{set type word{9896
{{mov{13,evvar(xr){21,=trbev{{set dummy trblk pointer{9897
{{lcw{8,wa{{{load exblk pointer{9898
{{mov{13,evexp(xr){8,wa{{set exblk pointer{9899
{{mov{7,xl{7,xr{{move name base to proper reg{9900
{{mov{8,wa{19,*evvar{{set name offset = zero{9901
{{brn{6,exnam{{{exit with name in (xl,wa){9902
{{ejc{{{{{9903
*      load pattern value
{o_lpt{ent{{{{entry point{9907
{{lcw{7,xr{{{load pattern pointer{9908
{{mov{11,-(xs){7,xr{{stack result{9909
{{lcw{7,xr{{{get next code word{9910
{{bri{9,(xr){{{execute it{9911
{{ejc{{{{{9912
*      load variable name
{o_lvn{ent{{{{entry point{9916
{{lcw{8,wa{{{load vrblk pointer{9917
{{mov{11,-(xs){8,wa{{stack vrblk ptr (name base){9918
{{mov{11,-(xs){19,*vrval{{stack name offset{9919
{{lcw{7,xr{{{get next code word{9920
{{bri{9,(xr){{{execute next code word{9921
{{ejc{{{{{9922
*      binary asterisk (multiplication)
{o_mlt{ent{{{{entry point{9926
{{jsr{6,arith{{{fetch arithmetic operands{9927
{{err{1,026{26,multiplication left operand is not numeric{{{9928
{{err{1,027{26,multiplication right operand is not numeric{{{9929
{{ppm{6,omlt1{{{jump if real operands{9932
*      here to multiply two integers
{{mli{13,icval(xl){{{multiply left operand by right{9937
{{ino{6,exint{{{return integer if no overflow{9938
{{erb{1,028{26,multiplication caused integer overflow{{{9939
*      here to multiply two reals
{omlt1{mlr{13,rcval(xl){{{multiply left operand by right{9945
{{rno{6,exrea{{{return real if no overflow{9946
{{erb{1,263{26,multiplication caused real overflow{{{9947
{{ejc{{{{{9949
*      name reference
{o_nam{ent{{{{entry point{9953
{{mov{8,wa{19,*nmsi_{{set length of nmblk{9954
{{jsr{6,alloc{{{allocate nmblk{9955
{{mov{9,(xr){22,=b_nml{{set name block code{9956
{{mov{13,nmofs(xr){10,(xs)+{{set name offset from operand{9957
{{mov{13,nmbas(xr){10,(xs)+{{set name base from operand{9958
{{mov{11,-(xs){7,xr{{stack result{9959
{{lcw{7,xr{{{get next code word{9960
{{bri{9,(xr){{{execute it{9961
{{ejc{{{{{9962
*      negation
*      initial entry
{o_nta{ent{{{{entry point{9968
{{lcw{8,wa{{{load new failure offset{9969
{{mov{11,-(xs){3,flptr{{stack old failure pointer{9970
{{mov{11,-(xs){8,wa{{stack new failure offset{9971
{{mov{3,flptr{7,xs{{set new failure pointer{9972
{{lcw{7,xr{{{get next code word{9973
{{bri{9,(xr){{{execute next code word{9974
*      entry after successful evaluation of operand
{o_ntb{ent{{{{entry point{9978
{{mov{3,flptr{13,num02(xs){{restore old failure pointer{9979
{{brn{6,exfal{{{and fail{9980
*      entry for failure during operand evaluation
{o_ntc{ent{{{{entry point{9984
{{ica{7,xs{{{pop failure offset{9985
{{mov{3,flptr{10,(xs)+{{restore old failure pointer{9986
{{brn{6,exnul{{{exit giving null result{9987
{{ejc{{{{{9988
*      use of undefined operator
{o_oun{ent{{{{entry point{9992
{{erb{1,029{26,undefined operator referenced{{{9993
{{ejc{{{{{9994
*      binary dot (pattern assignment)
*      the pattern built by binary dot is a compound pattern.
*      see description at start of pattern match section for
*      details of the structure which is constructed.
{o_pas{ent{{{{entry point{10002
{{mov{8,wb{22,=p_pac{{load pcode for p_pac node{10003
{{mov{8,wc{10,(xs)+{{load name offset (parm2){10004
{{mov{7,xr{10,(xs)+{{load name base (parm1){10005
{{jsr{6,pbild{{{build p_pac node{10006
{{mov{7,xl{7,xr{{save ptr to node{10007
{{mov{7,xr{9,(xs){{load left operand{10008
{{jsr{6,gtpat{{{convert to pattern{10009
{{err{1,030{26,pattern assignment left operand is not pattern{{{10010
{{mov{9,(xs){7,xr{{save ptr to left operand pattern{10011
{{mov{8,wb{22,=p_paa{{set pcode for p_paa node{10012
{{jsr{6,pbild{{{build p_paa node{10013
{{mov{13,pthen(xr){10,(xs)+{{set left operand as p_paa successor{10014
{{jsr{6,pconc{{{concatenate to form final pattern{10015
{{mov{11,-(xs){7,xr{{stack result{10016
{{lcw{7,xr{{{get next code word{10017
{{bri{9,(xr){{{execute it{10018
{{ejc{{{{{10019
*      pattern match (by name, for replacement)
{o_pmn{ent{{{{entry point{10023
{{zer{8,wb{{{set type code for match by name{10024
{{brn{6,match{{{jump to routine to start match{10025
{{ejc{{{{{10026
*      pattern match (statement)
*      o_pms is used in place of o_pmv when the pattern match
*      occurs at the outer (statement) level since in this
*      case the substring value need not be constructed.
{o_pms{ent{{{{entry point{10034
{{mov{8,wb{18,=num02{{set flag for statement to match{10035
{{brn{6,match{{{jump to routine to start match{10036
{{ejc{{{{{10037
*      pattern match (by value)
{o_pmv{ent{{{{entry point{10041
{{mov{8,wb{18,=num01{{set type code for value match{10042
{{brn{6,match{{{jump to routine to start match{10043
{{ejc{{{{{10044
*      pop top item on stack
{o_pop{ent{{{{entry point{10048
{{ica{7,xs{{{pop top stack entry{10049
{{lcw{7,xr{{{get next code word{10050
{{bri{9,(xr){{{execute next code word{10051
{{ejc{{{{{10052
*      terminate execution (code compiled for end statement)
{o_stp{ent{{{{entry point{10056
{{brn{6,lend0{{{jump to end circuit{10057
{{ejc{{{{{10058
*      return name from expression
*      this entry points is used if the evaluation of an
*      expression, initiated by the evalx procedure, returns
*      a name. control is returned to the proper point in evalx.
{o_rnm{ent{{{{entry point{10065
{{brn{6,evlx4{{{return to evalx procedure{10066
{{ejc{{{{{10067
*      pattern replacement
*      when this routine gets control, the following stack
*      entries have been made (see end of match routine p_nth)
*                            subject name base
*                            subject name offset
*                            initial cursor value
*                            final cursor value
*                            subject string pointer
*      (xs) ---------------- replacement value
{o_rpl{ent{{{{entry point{10081
{{jsr{6,gtstg{{{convert replacement val to string{10082
{{err{1,031{26,pattern replacement right operand is not a string{{{10083
*      get result length and allocate result scblk
{{mov{7,xl{9,(xs){{load subject string pointer{10087
{{add{8,wa{13,sclen(xl){{add subject string length{10092
{{add{8,wa{13,num02(xs){{add starting cursor{10093
{{sub{8,wa{13,num01(xs){{minus final cursor = total length{10094
{{bze{8,wa{6,orpl3{{jump if result is null{10095
{{mov{11,-(xs){7,xr{{restack replacement string{10096
{{jsr{6,alocs{{{allocate scblk for result{10097
{{mov{8,wa{13,num03(xs){{get initial cursor (part 1 len){10098
{{mov{13,num03(xs){7,xr{{stack result pointer{10099
{{psc{7,xr{{{point to characters of result{10100
*      move part 1 (start of subject) to result
{{bze{8,wa{6,orpl1{{jump if first part is null{10104
{{mov{7,xl{13,num01(xs){{else point to subject string{10105
{{plc{7,xl{{{point to subject string chars{10106
{{mvc{{{{move first part to result{10107
{{ejc{{{{{10108
*      pattern replacement (continued)
*      now move in replacement value
{orpl1{mov{7,xl{10,(xs)+{{load replacement string, pop{10113
{{mov{8,wa{13,sclen(xl){{load length{10114
{{bze{8,wa{6,orpl2{{jump if null replacement{10115
{{plc{7,xl{{{else point to chars of replacement{10116
{{mvc{{{{move in chars (part 2){10117
*      now move in remainder of string (part 3)
{orpl2{mov{7,xl{10,(xs)+{{load subject string pointer, pop{10121
{{mov{8,wc{10,(xs)+{{load final cursor, pop{10122
{{mov{8,wa{13,sclen(xl){{load subject string length{10123
{{sub{8,wa{8,wc{{minus final cursor = part 3 length{10124
{{bze{8,wa{6,oass0{{jump to assign if part 3 is null{10125
{{plc{7,xl{8,wc{{else point to last part of string{10126
{{mvc{{{{move part 3 to result{10127
{{brn{6,oass0{{{jump to perform assignment{10128
*      here if result is null
{orpl3{add{7,xs{19,*num02{{pop subject str ptr, final cursor{10132
{{mov{9,(xs){21,=nulls{{set null result{10133
{{brn{6,oass0{{{jump to assign null value{10134
{{ejc{{{{{10153
*      return value from expression
*      this entry points is used if the evaluation of an
*      expression, initiated by the evalx procedure, returns
*      a value. control is returned to the proper point in evalx
{o_rvl{ent{{{{entry point{10161
{{brn{6,evlx3{{{return to evalx procedure{10162
{{ejc{{{{{10163
*      selection
*      initial entry
{o_sla{ent{{{{entry point{10169
{{lcw{8,wa{{{load new failure offset{10170
{{mov{11,-(xs){3,flptr{{stack old failure pointer{10171
{{mov{11,-(xs){8,wa{{stack new failure offset{10172
{{mov{3,flptr{7,xs{{set new failure pointer{10173
{{lcw{7,xr{{{get next code word{10174
{{bri{9,(xr){{{execute next code word{10175
*      entry after successful evaluation of alternative
{o_slb{ent{{{{entry point{10179
{{mov{7,xr{10,(xs)+{{load result{10180
{{ica{7,xs{{{pop fail offset{10181
{{mov{3,flptr{9,(xs){{restore old failure pointer{10182
{{mov{9,(xs){7,xr{{restack result{10183
{{lcw{8,wa{{{load new code offset{10184
{{add{8,wa{3,r_cod{{point to absolute code location{10185
{{lcp{8,wa{{{set new code pointer{10186
{{lcw{7,xr{{{get next code word{10187
{{bri{9,(xr){{{execute next code word{10188
*      entry at start of subsequent alternatives
{o_slc{ent{{{{entry point{10192
{{lcw{8,wa{{{load new fail offset{10193
{{mov{9,(xs){8,wa{{store new fail offset{10194
{{lcw{7,xr{{{get next code word{10195
{{bri{9,(xr){{{execute next code word{10196
*      entry at start of last alternative
{o_sld{ent{{{{entry point{10200
{{ica{7,xs{{{pop failure offset{10201
{{mov{3,flptr{10,(xs)+{{restore old failure pointer{10202
{{lcw{7,xr{{{get next code word{10203
{{bri{9,(xr){{{execute next code word{10204
{{ejc{{{{{10205
*      binary minus (subtraction)
{o_sub{ent{{{{entry point{10209
{{jsr{6,arith{{{fetch arithmetic operands{10210
{{err{1,032{26,subtraction left operand is not numeric{{{10211
{{err{1,033{26,subtraction right operand is not numeric{{{10212
{{ppm{6,osub1{{{jump if real operands{10215
*      here to subtract two integers
{{sbi{13,icval(xl){{{subtract right operand from left{10220
{{ino{6,exint{{{return integer if no overflow{10221
{{erb{1,034{26,subtraction caused integer overflow{{{10222
*      here to subtract two reals
{osub1{sbr{13,rcval(xl){{{subtract right operand from left{10228
{{rno{6,exrea{{{return real if no overflow{10229
{{erb{1,264{26,subtraction caused real overflow{{{10230
{{ejc{{{{{10232
*      dummy operator to return control to trxeq procedure
{o_txr{ent{{{{entry point{10236
{{brn{6,trxq1{{{jump into trxeq procedure{10237
{{ejc{{{{{10238
*      unexpected failure
*      note that if a setexit trap is operating then
*      transfer to system label continue
*      will result in looping here.  difficult to avoid except
*      with a considerable overhead which is not worthwhile or
*      else by a technique such as setting kverl to zero.
{o_unf{ent{{{{entry point{10248
{{erb{1,035{26,unexpected failure in -nofail mode{{{10249
{{ttl{27,s p i t b o l -- block action routines{{{{10250
*      the first word of every block in dynamic storage and the
*      vrget, vrsto and vrtra fields of a vrblk contain a
*      pointer to an entry point in the program. all such entry
*      points are in the following section except those for
*      pattern blocks which are in the pattern matching segment
*      later on (labels of the form p_xxx), and dope vectors
*      (d_xxx) which are in the dope vector section following
*      the pattern routines (dope vectors are used for cmblks).
*      the entry points in this section have labels of the
*      form b_xxy where xx is the two character block type for
*      the corresponding block and y is any letter.
*      in some cases, the pointers serve no other purpose than
*      to identify the block type. in this case the routine
*      is never executed and thus no code is assembled.
*      for each of these entry points corresponding to a block
*      an entry point identification is assembled (bl_xx).
*      the exact entry conditions depend on the manner in
*      which the routine is accessed and are documented with
*      the individual routines as required.
*      the order of these routines is alphabetical with the
*      following exceptions.
*      the routines for seblk and exblk entries occur first so
*      that expressions can be quickly identified from the fact
*      that their routines lie before the symbol b_e__.
*      these are immediately followed by the routine for a trblk
*      so that the test against the symbol b_t__ checks for
*      trapped values or expression values (see procedure evalp)
*      the pattern routines lie after this section so that
*      patterns are identified with routines starting at or
*      after the initial instruction in these routines (p_aaa).
*      the symbol b_aaa defines the first location for block
*      routines and the symbol p_yyy (at the end of the pattern
*      match routines section) defines the last such entry point
{b_aaa{ent{2,bl__i{{{entry point of first block routine{10295
{{ejc{{{{{10296
*      exblk
*      the routine for an exblk loads the expression onto
*      the stack as a value.
*      (xr)                  pointer to exblk
{b_exl{ent{2,bl_ex{{{entry point (exblk){10305
{{mov{11,-(xs){7,xr{{stack result{10306
{{lcw{7,xr{{{get next code word{10307
{{bri{9,(xr){{{execute it{10308
{{ejc{{{{{10309
*      seblk
*      the routine for seblk is accessed from the generated
*      code to load the expression value onto the stack.
{b_sel{ent{2,bl_se{{{entry point (seblk){10316
{{mov{11,-(xs){7,xr{{stack result{10317
{{lcw{7,xr{{{get next code word{10318
{{bri{9,(xr){{{execute it{10319
*      define symbol which marks end of entries for expressions
{b_e__{ent{2,bl__i{{{entry point{10323
{{ejc{{{{{10324
*      trblk
*      the routine for a trblk is never executed
{b_trt{ent{2,bl_tr{{{entry point (trblk){10330
*      define symbol marking end of trap and expression blocks
{b_t__{ent{2,bl__i{{{end of trblk,seblk,exblk entries{10334
{{ejc{{{{{10335
*      arblk
*      the routine for arblk is never executed
{b_art{ent{2,bl_ar{{{entry point (arblk){10341
{{ejc{{{{{10342
*      bcblk
*      the routine for a bcblk is never executed
*      (xr)                  pointer to bcblk
{b_bct{ent{2,bl_bc{{{entry point (bcblk){10350
{{ejc{{{{{10351
*      bfblk
*      the routine for a bfblk is never executed
*      (xr)                  pointer to bfblk
{b_bft{ent{2,bl_bf{{{entry point (bfblk){10359
{{ejc{{{{{10360
*      ccblk
*      the routine for ccblk is never entered
{b_cct{ent{2,bl_cc{{{entry point (ccblk){10366
{{ejc{{{{{10367
*      cdblk
*      the cdblk routines are executed from the generated code.
*      there are two cases depending on the form of cdfal.
*      entry for complex failure code at cdfal
*      (xr)                  pointer to cdblk
{b_cdc{ent{2,bl_cd{{{entry point (cdblk){10378
{bcdc0{mov{7,xs{3,flptr{{pop garbage off stack{10379
{{mov{9,(xs){13,cdfal(xr){{set failure offset{10380
{{brn{6,stmgo{{{enter stmt{10381
{{ejc{{{{{10382
*      cdblk (continued)
*      entry for simple failure code at cdfal
*      (xr)                  pointer to cdblk
{b_cds{ent{2,bl_cd{{{entry point (cdblk){10390
{bcds0{mov{7,xs{3,flptr{{pop garbage off stack{10391
{{mov{9,(xs){19,*cdfal{{set failure offset{10392
{{brn{6,stmgo{{{enter stmt{10393
{{ejc{{{{{10394
*      cmblk
*      the routine for a cmblk is never executed
{b_cmt{ent{2,bl_cm{{{entry point (cmblk){10400
{{ejc{{{{{10401
*      ctblk
*      the routine for a ctblk is never executed
{b_ctt{ent{2,bl_ct{{{entry point (ctblk){10407
{{ejc{{{{{10408
*      dfblk
*      the routine for a dfblk is accessed from the o_fnc entry
*      to call a datatype function and build a pdblk.
*      (xl)                  pointer to dfblk
{b_dfc{ent{2,bl_df{{{entry point{10417
{{mov{8,wa{13,dfpdl(xl){{load length of pdblk{10418
{{jsr{6,alloc{{{allocate pdblk{10419
{{mov{9,(xr){22,=b_pdt{{store type word{10420
{{mov{13,pddfp(xr){7,xl{{store dfblk pointer{10421
{{mov{8,wc{7,xr{{save pointer to pdblk{10422
{{add{7,xr{8,wa{{point past pdblk{10423
{{lct{8,wa{13,fargs(xl){{set to count fields{10424
*      loop to acquire field values from stack
{bdfc1{mov{11,-(xr){10,(xs)+{{move a field value{10428
{{bct{8,wa{6,bdfc1{{loop till all moved{10429
{{mov{7,xr{8,wc{{recall pointer to pdblk{10430
{{brn{6,exsid{{{exit setting id field{10431
{{ejc{{{{{10432
*      efblk
*      the routine for an efblk is passed control form the o_fnc
*      entry to call an external function.
*      (xl)                  pointer to efblk
{b_efc{ent{2,bl_ef{{{entry point (efblk){10441
{{mov{8,wc{13,fargs(xl){{load number of arguments{10444
{{wtb{8,wc{{{convert to offset{10445
{{mov{11,-(xs){7,xl{{save pointer to efblk{10446
{{mov{7,xt{7,xs{{copy pointer to arguments{10447
*      loop to convert arguments
{befc1{ica{7,xt{{{point to next entry{10451
{{mov{7,xr{9,(xs){{load pointer to efblk{10452
{{dca{8,wc{{{decrement eftar offset{10453
{{add{7,xr{8,wc{{point to next eftar entry{10454
{{mov{7,xr{13,eftar(xr){{load eftar entry{10455
{{bsw{7,xr{1,5{{switch on type{10464
{{iff{1,0{6,befc7{{no conversion needed{10482
{{iff{1,1{6,befc2{{string{10482
{{iff{1,2{6,befc3{{integer{10482
{{iff{1,3{6,befc4{{real{10482
{{iff{1,4{6,beff1{{file{10482
{{esw{{{{end of switch on type{10482
*      here to convert to file
{beff1{mov{11,-(xs){7,xt{{save entry pointer{10487
{{mov{3,befof{8,wc{{save offset{10488
{{mov{11,-(xs){9,(xt){{stack arg pointer{10489
{{jsr{6,iofcb{{{convert to fcb{10490
{{err{1,298{26,external function argument is not file{{{10491
{{err{1,298{26,external function argument is not file{{{10492
{{err{1,298{26,external function argument is not file{{{10493
{{mov{7,xr{8,wa{{point to fcb{10494
{{mov{7,xt{10,(xs)+{{reload entry pointer{10495
{{brn{6,befc5{{{jump to merge{10496
*      here to convert to string
{befc2{mov{11,-(xs){9,(xt){{stack arg ptr{10501
{{jsr{6,gtstg{{{convert argument to string{10502
{{err{1,039{26,external function argument is not a string{{{10503
{{brn{6,befc6{{{jump to merge{10504
{{ejc{{{{{10505
*      efblk (continued)
*      here to convert an integer
{befc3{mov{7,xr{9,(xt){{load next argument{10511
{{mov{3,befof{8,wc{{save offset{10512
{{jsr{6,gtint{{{convert to integer{10513
{{err{1,040{26,external function argument is not integer{{{10514
{{brn{6,befc5{{{merge with real case{10517
*      here to convert a real
{befc4{mov{7,xr{9,(xt){{load next argument{10521
{{mov{3,befof{8,wc{{save offset{10522
{{jsr{6,gtrea{{{convert to real{10523
{{err{1,265{26,external function argument is not real{{{10524
*      integer case merges here
{befc5{mov{8,wc{3,befof{{restore offset{10529
*      string merges here
{befc6{mov{9,(xt){7,xr{{store converted result{10533
*      no conversion merges here
{befc7{bnz{8,wc{6,befc1{{loop back if more to go{10537
*      here after converting all the arguments
{{mov{7,xl{10,(xs)+{{restore efblk pointer{10541
{{mov{8,wa{13,fargs(xl){{get number of args{10542
{{jsr{6,sysex{{{call routine to call external fnc{10543
{{ppm{6,exfal{{{fail if failure{10544
{{err{1,327{26,calling external function - not found{{{10545
{{err{1,326{26,calling external function - bad argument type{{{10546
{{wtb{8,wa{{{convert number of args to bytes{10548
{{add{7,xs{8,wa{{remove arguments from stack{10549
{{ejc{{{{{10551
*      efblk (continued)
*      return here with result in xr
*      first defend against non-standard null string returned
{{mov{8,wb{13,efrsl(xl){{get result type id{10559
{{bnz{8,wb{6,befa8{{branch if not unconverted{10560
{{bne{9,(xr){22,=b_scl{6,befc8{jump if not a string{10561
{{bze{13,sclen(xr){6,exnul{{return null if null{10562
*      here if converted result to check for null string
{befa8{bne{8,wb{18,=num01{6,befc8{jump if not a string{10566
{{bze{13,sclen(xr){6,exnul{{return null if null{10567
*      return if result is in dynamic storage
{befc8{blt{7,xr{3,dnamb{6,befc9{jump if not in dynamic storage{10571
{{ble{7,xr{3,dnamp{6,exixr{return result if already dynamic{10572
*      here we copy a result into the dynamic region
{befc9{mov{8,wa{9,(xr){{get possible type word{10576
{{bze{8,wb{6,bef11{{jump if unconverted result{10577
{{mov{8,wa{22,=b_scl{{string{10578
{{beq{8,wb{18,=num01{6,bef10{yes jump{10579
{{mov{8,wa{22,=b_icl{{integer{10580
{{beq{8,wb{18,=num02{6,bef10{yes jump{10581
{{mov{8,wa{22,=b_rcl{{real{10584
*      store type word in result
{bef10{mov{9,(xr){8,wa{{stored before copying to dynamic{10589
*      merge for unconverted result
{bef11{beq{9,(xr){22,=b_scl{6,bef12{branch if string result{10593
{{jsr{6,blkln{{{get length of block{10594
{{mov{7,xl{7,xr{{copy address of old block{10595
{{jsr{6,alloc{{{allocate dynamic block same size{10596
{{mov{11,-(xs){7,xr{{set pointer to new block as result{10597
{{mvw{{{{copy old block to dynamic block{10598
{{zer{7,xl{{{clear garbage value{10599
{{lcw{7,xr{{{get next code word{10600
{{bri{9,(xr){{{execute next code word{10601
*      here to return a string result that was not in dynamic.
*      cannot use the simple word copy above because it will not
*      guarantee zero padding in the last word.
{bef12{mov{7,xl{7,xr{{save source string pointer{10607
{{mov{8,wa{13,sclen(xr){{fetch string length{10608
{{bze{8,wa{6,exnul{{return null string if length zero{10609
{{jsr{6,alocs{{{allocate space for string{10610
{{mov{11,-(xs){7,xr{{save as result pointer{10611
{{psc{7,xr{{{prepare to store chars of result{10612
{{plc{7,xl{{{point to chars in source string{10613
{{mov{8,wa{8,wc{{number of characters to copy{10614
{{mvc{{{{move characters to result string{10615
{{zer{7,xl{{{clear garbage value{10616
{{lcw{7,xr{{{get next code word{10617
{{bri{9,(xr){{{execute next code word{10618
{{ejc{{{{{10620
*      evblk
*      the routine for an evblk is never executed
{b_evt{ent{2,bl_ev{{{entry point (evblk){10626
{{ejc{{{{{10627
*      ffblk
*      the routine for an ffblk is executed from the o_fnc entry
*      to call a field function and extract a field value/name.
*      (xl)                  pointer to ffblk
{b_ffc{ent{2,bl_ff{{{entry point (ffblk){10636
{{mov{7,xr{7,xl{{copy ffblk pointer{10637
{{lcw{8,wc{{{load next code word{10638
{{mov{7,xl{9,(xs){{load pdblk pointer{10639
{{bne{9,(xl){22,=b_pdt{6,bffc2{jump if not pdblk at all{10640
{{mov{8,wa{13,pddfp(xl){{load dfblk pointer from pdblk{10641
*      loop to find correct ffblk for this pdblk
{bffc1{beq{8,wa{13,ffdfp(xr){6,bffc3{jump if this is the correct ffblk{10645
{{mov{7,xr{13,ffnxt(xr){{else link to next ffblk on chain{10646
{{bnz{7,xr{6,bffc1{{loop back if another entry to check{10647
*      here for bad argument
{bffc2{erb{1,041{26,field function argument is wrong datatype{{{10651
{{ejc{{{{{10652
*      ffblk (continued)
*      here after locating correct ffblk
{bffc3{mov{8,wa{13,ffofs(xr){{load field offset{10658
{{beq{8,wc{21,=ofne_{6,bffc5{jump if called by name{10659
{{add{7,xl{8,wa{{else point to value field{10660
{{mov{7,xr{9,(xl){{load value{10661
{{bne{9,(xr){22,=b_trt{6,bffc4{jump if not trapped{10662
{{sub{7,xl{8,wa{{else restore name base,offset{10663
{{mov{9,(xs){8,wc{{save next code word over pdblk ptr{10664
{{jsr{6,acess{{{access value{10665
{{ppm{6,exfal{{{fail if access fails{10666
{{mov{8,wc{9,(xs){{restore next code word{10667
*      here after getting value in (xr), xl is garbage
{bffc4{mov{9,(xs){7,xr{{store value on stack (over pdblk){10671
{{mov{7,xr{8,wc{{copy next code word{10672
{{mov{7,xl{9,(xr){{load entry address{10673
{{bri{7,xl{{{jump to routine for next code word{10674
*      here if called by name
{bffc5{mov{11,-(xs){8,wa{{store name offset (base is set){10678
{{lcw{7,xr{{{get next code word{10679
{{bri{9,(xr){{{execute next code word{10680
{{ejc{{{{{10681
*      icblk
*      the routine for icblk is executed from the generated
*      code to load an integer value onto the stack.
*      (xr)                  pointer to icblk
{b_icl{ent{2,bl_ic{{{entry point (icblk){10690
{{mov{11,-(xs){7,xr{{stack result{10691
{{lcw{7,xr{{{get next code word{10692
{{bri{9,(xr){{{execute it{10693
{{ejc{{{{{10694
*      kvblk
*      the routine for a kvblk is never executed.
{b_kvt{ent{2,bl_kv{{{entry point (kvblk){10700
{{ejc{{{{{10701
*      nmblk
*      the routine for a nmblk is executed from the generated
*      code for the case of loading a name onto the stack
*      where the name is that of a natural variable which can
*      be preevaluated at compile time.
*      (xr)                  pointer to nmblk
{b_nml{ent{2,bl_nm{{{entry point (nmblk){10712
{{mov{11,-(xs){7,xr{{stack result{10713
{{lcw{7,xr{{{get next code word{10714
{{bri{9,(xr){{{execute it{10715
{{ejc{{{{{10716
*      pdblk
*      the routine for a pdblk is never executed
{b_pdt{ent{2,bl_pd{{{entry point (pdblk){10722
{{ejc{{{{{10723
*      pfblk
*      the routine for a pfblk is executed from the entry o_fnc
*      to call a program defined function.
*      (xl)                  pointer to pfblk
*      the following stack entries are made before passing
*      control to the program defined function.
*                            saved value of first argument
*                            .
*                            saved value of last argument
*                            saved value of first local
*                            .
*                            saved value of last local
*                            saved value of function name
*                            saved code block ptr (r_cod)
*                            saved code pointer (-r_cod)
*                            saved value of flprt
*                            saved value of flptr
*                            pointer to pfblk
*      flptr --------------- zero (to be overwritten with offs)
{b_pfc{ent{2,bl_pf{{{entry point (pfblk){10749
{{mov{3,bpfpf{7,xl{{save pfblk ptr (need not be reloc){10750
{{mov{7,xr{7,xl{{copy for the moment{10751
{{mov{7,xl{13,pfvbl(xr){{point to vrblk for function{10752
*      loop to find old value of function
{bpf01{mov{8,wb{7,xl{{save pointer{10756
{{mov{7,xl{13,vrval(xl){{load value{10757
{{beq{9,(xl){22,=b_trt{6,bpf01{loop if trblk{10758
*      set value to null and save old function value
{{mov{3,bpfsv{7,xl{{save old value{10762
{{mov{7,xl{8,wb{{point back to block with value{10763
{{mov{13,vrval(xl){21,=nulls{{set value to null{10764
{{mov{8,wa{13,fargs(xr){{load number of arguments{10765
{{add{7,xr{19,*pfarg{{point to pfarg entries{10766
{{bze{8,wa{6,bpf04{{jump if no arguments{10767
{{mov{7,xt{7,xs{{ptr to last arg{10768
{{wtb{8,wa{{{convert no. of args to bytes offset{10769
{{add{7,xt{8,wa{{point before first arg{10770
{{mov{3,bpfxt{7,xt{{remember arg pointer{10771
{{ejc{{{{{10772
*      pfblk (continued)
*      loop to save old argument values and set new ones
{bpf02{mov{7,xl{10,(xr)+{{load vrblk ptr for next argument{10778
*      loop through possible trblk chain to find value
{bpf03{mov{8,wc{7,xl{{save pointer{10782
{{mov{7,xl{13,vrval(xl){{load next value{10783
{{beq{9,(xl){22,=b_trt{6,bpf03{loop back if trblk{10784
*      save old value and get new value
{{mov{8,wa{7,xl{{keep old value{10788
{{mov{7,xt{3,bpfxt{{point before next stacked arg{10789
{{mov{8,wb{11,-(xt){{load argument (new value){10790
{{mov{9,(xt){8,wa{{save old value{10791
{{mov{3,bpfxt{7,xt{{keep arg ptr for next time{10792
{{mov{7,xl{8,wc{{point back to block with value{10793
{{mov{13,vrval(xl){8,wb{{set new value{10794
{{bne{7,xs{3,bpfxt{6,bpf02{loop if not all done{10795
*      now process locals
{bpf04{mov{7,xl{3,bpfpf{{restore pfblk pointer{10799
{{mov{8,wa{13,pfnlo(xl){{load number of locals{10800
{{bze{8,wa{6,bpf07{{jump if no locals{10801
{{mov{8,wb{21,=nulls{{get null constant{10802
{{lct{8,wa{8,wa{{set local counter{10803
*      loop to process locals
{bpf05{mov{7,xl{10,(xr)+{{load vrblk ptr for next local{10807
*      loop through possible trblk chain to find value
{bpf06{mov{8,wc{7,xl{{save pointer{10811
{{mov{7,xl{13,vrval(xl){{load next value{10812
{{beq{9,(xl){22,=b_trt{6,bpf06{loop back if trblk{10813
*      save old value and set null as new value
{{mov{11,-(xs){7,xl{{stack old value{10817
{{mov{7,xl{8,wc{{point back to block with value{10818
{{mov{13,vrval(xl){8,wb{{set null as new value{10819
{{bct{8,wa{6,bpf05{{loop till all locals processed{10820
{{ejc{{{{{10821
*      pfblk (continued)
*      here after processing arguments and locals
{bpf07{zer{7,xr{{{zero reg xr in case{10830
{{bze{3,kvpfl{6,bpf7c{{skip if profiling is off{10831
{{beq{3,kvpfl{18,=num02{6,bpf7a{branch on type of profile{10832
*      here if &profile = 1
{{jsr{6,systm{{{get current time{10836
{{sti{3,pfetm{{{save for a sec{10837
{{sbi{3,pfstm{{{find time used by caller{10838
{{jsr{6,icbld{{{build into an icblk{10839
{{ldi{3,pfetm{{{reload current time{10840
{{brn{6,bpf7b{{{merge{10841
*       here if &profile = 2
{bpf7a{ldi{3,pfstm{{{get start time of calling stmt{10845
{{jsr{6,icbld{{{assemble an icblk round it{10846
{{jsr{6,systm{{{get now time{10847
*      both types of profile merge here
{bpf7b{sti{3,pfstm{{{set start time of 1st func stmt{10851
{{mnz{3,pffnc{{{flag function entry{10852
*      no profiling merges here
{bpf7c{mov{11,-(xs){7,xr{{stack icblk ptr (or zero){10856
{{mov{8,wa{3,r_cod{{load old code block pointer{10857
{{scp{8,wb{{{get code pointer{10859
{{sub{8,wb{8,wa{{make code pointer into offset{10860
{{mov{7,xl{3,bpfpf{{recall pfblk pointer{10861
{{mov{11,-(xs){3,bpfsv{{stack old value of function name{10862
{{mov{11,-(xs){8,wa{{stack code block pointer{10863
{{mov{11,-(xs){8,wb{{stack code offset{10864
{{mov{11,-(xs){3,flprt{{stack old flprt{10865
{{mov{11,-(xs){3,flptr{{stack old failure pointer{10866
{{mov{11,-(xs){7,xl{{stack pointer to pfblk{10867
{{zer{11,-(xs){{{dummy zero entry for fail return{10868
{{chk{{{{check for stack overflow{10869
{{mov{3,flptr{7,xs{{set new fail return value{10870
{{mov{3,flprt{7,xs{{set new flprt{10871
{{mov{8,wa{3,kvtra{{load trace value{10872
{{add{8,wa{3,kvftr{{add ftrace value{10873
{{bnz{8,wa{6,bpf09{{jump if tracing possible{10874
{{icv{3,kvfnc{{{else bump fnclevel{10875
*      here to actually jump to function
{bpf08{mov{7,xr{13,pfcod(xl){{point to vrblk of entry label{10879
{{mov{7,xr{13,vrlbl(xr){{point to target code{10880
{{beq{7,xr{21,=stndl{6,bpf17{test for undefined label{10881
{{bne{9,(xr){22,=b_trt{6,bpf8a{jump if not trapped{10882
{{mov{7,xr{13,trlbl(xr){{else load ptr to real label code{10883
{bpf8a{bri{9,(xr){{{off to execute function{10884
*      here if tracing is possible
{bpf09{mov{7,xr{13,pfctr(xl){{load possible call trace trblk{10888
{{mov{7,xl{13,pfvbl(xl){{load vrblk pointer for function{10889
{{mov{8,wa{19,*vrval{{set name offset for variable{10890
{{bze{3,kvtra{6,bpf10{{jump if trace mode is off{10891
{{bze{7,xr{6,bpf10{{or if there is no call trace{10892
*      here if call traced
{{dcv{3,kvtra{{{decrement trace count{10896
{{bze{13,trfnc(xr){6,bpf11{{jump if print trace{10897
{{jsr{6,trxeq{{{execute function type trace{10898
{{ejc{{{{{10899
*      pfblk (continued)
*      here to test for ftrace trace
{bpf10{bze{3,kvftr{6,bpf16{{jump if ftrace is off{10905
{{dcv{3,kvftr{{{else decrement ftrace{10906
*      here for print trace
{bpf11{jsr{6,prtsn{{{print statement number{10910
{{jsr{6,prtnm{{{print function name{10911
{{mov{8,wa{18,=ch_pp{{load left paren{10912
{{jsr{6,prtch{{{print left paren{10913
{{mov{7,xl{13,num01(xs){{recover pfblk pointer{10914
{{bze{13,fargs(xl){6,bpf15{{skip if no arguments{10915
{{zer{8,wb{{{else set argument counter{10916
{{brn{6,bpf13{{{jump into loop{10917
*      loop to print argument values
{bpf12{mov{8,wa{18,=ch_cm{{load comma{10921
{{jsr{6,prtch{{{print to separate from last arg{10922
*      merge here first time (no comma required)
{bpf13{mov{9,(xs){8,wb{{save arg ctr (over failoffs is ok){10926
{{wtb{8,wb{{{convert to byte offset{10927
{{add{7,xl{8,wb{{point to next argument pointer{10928
{{mov{7,xr{13,pfarg(xl){{load next argument vrblk ptr{10929
{{sub{7,xl{8,wb{{restore pfblk pointer{10930
{{mov{7,xr{13,vrval(xr){{load next value{10931
{{jsr{6,prtvl{{{print argument value{10932
{{ejc{{{{{10933
*      here after dealing with one argument
{{mov{8,wb{9,(xs){{restore argument counter{10937
{{icv{8,wb{{{increment argument counter{10938
{{blt{8,wb{13,fargs(xl){6,bpf12{loop if more to print{10939
*      merge here in no args case to print paren
{bpf15{mov{8,wa{18,=ch_rp{{load right paren{10943
{{jsr{6,prtch{{{print to terminate output{10944
{{jsr{6,prtnl{{{terminate print line{10945
*      merge here to exit with test for fnclevel trace
{bpf16{icv{3,kvfnc{{{increment fnclevel{10949
{{mov{7,xl{3,r_fnc{{load ptr to possible trblk{10950
{{jsr{6,ktrex{{{call keyword trace routine{10951
*      call function after trace tests complete
{{mov{7,xl{13,num01(xs){{restore pfblk pointer{10955
{{brn{6,bpf08{{{jump back to execute function{10956
*      here if calling a function whose entry label is undefined
{bpf17{mov{3,flptr{13,num02(xs){{reset so exfal can return to evalx{10960
{{erb{1,286{26,function call to undefined entry label{{{10961
{{ejc{{{{{10964
*      rcblk
*      the routine for an rcblk is executed from the generated
*      code to load a real value onto the stack.
*      (xr)                  pointer to rcblk
{b_rcl{ent{2,bl_rc{{{entry point (rcblk){10973
{{mov{11,-(xs){7,xr{{stack result{10974
{{lcw{7,xr{{{get next code word{10975
{{bri{9,(xr){{{execute it{10976
{{ejc{{{{{10978
*      scblk
*      the routine for an scblk is executed from the generated
*      code to load a string value onto the stack.
*      (xr)                  pointer to scblk
{b_scl{ent{2,bl_sc{{{entry point (scblk){10987
{{mov{11,-(xs){7,xr{{stack result{10988
{{lcw{7,xr{{{get next code word{10989
{{bri{9,(xr){{{execute it{10990
{{ejc{{{{{10991
*      tbblk
*      the routine for a tbblk is never executed
{b_tbt{ent{2,bl_tb{{{entry point (tbblk){10997
{{ejc{{{{{10998
*      teblk
*      the routine for a teblk is never executed
{b_tet{ent{2,bl_te{{{entry point (teblk){11004
{{ejc{{{{{11005
*      vcblk
*      the routine for a vcblk is never executed
{b_vct{ent{2,bl_vc{{{entry point (vcblk){11011
{{ejc{{{{{11012
*      vrblk
*      the vrblk routines are executed from the generated code.
*      there are six entries for vrblk covering various cases
{b_vr_{ent{2,bl__i{{{mark start of vrblk entry points{11019
*      entry for vrget (trapped case). this routine is called
*      from the generated code to load the value of a variable.
*      this entry point is used if an access trace or input
*      association is currently active.
*      (xr)                  pointer to vrget field of vrblk
{b_vra{ent{2,bl__i{{{entry point{11028
{{mov{7,xl{7,xr{{copy name base (vrget = 0){11029
{{mov{8,wa{19,*vrval{{set name offset{11030
{{jsr{6,acess{{{access value{11031
{{ppm{6,exfal{{{fail if access fails{11032
{{mov{11,-(xs){7,xr{{stack result{11033
{{lcw{7,xr{{{get next code word{11034
{{bri{9,(xr){{{execute it{11035
{{ejc{{{{{11036
*      vrblk (continued)
*      entry for vrsto (error case. this routine is called from
*      the executed code for an attempt to modify the value
*      of a protected (pattern valued) natural variable.
{b_vre{ent{{{{entry point{11044
{{erb{1,042{26,attempt to change value of protected variable{{{11045
{{ejc{{{{{11046
*      vrblk (continued)
*      entry for vrtra (untrapped case). this routine is called
*      from the executed code to transfer to a label.
*      (xr)                  pointer to vrtra field of vrblk
{b_vrg{ent{{{{entry point{11055
{{mov{7,xr{13,vrlbo(xr){{load code pointer{11056
{{mov{7,xl{9,(xr){{load entry address{11057
{{bri{7,xl{{{jump to routine for next code word{11058
{{ejc{{{{{11059
*      vrblk (continued)
*      entry for vrget (untrapped case). this routine is called
*      from the generated code to load the value of a variable.
*      (xr)                  points to vrget field of vrblk
{b_vrl{ent{{{{entry point{11068
{{mov{11,-(xs){13,vrval(xr){{load value onto stack (vrget = 0){11069
{{lcw{7,xr{{{get next code word{11070
{{bri{9,(xr){{{execute next code word{11071
{{ejc{{{{{11072
*      vrblk (continued)
*      entry for vrsto (untrapped case). this routine is called
*      from the generated code to store the value of a variable.
*      (xr)                  pointer to vrsto field of vrblk
{b_vrs{ent{{{{entry point{11081
{{mov{13,vrvlo(xr){9,(xs){{store value, leave on stack{11082
{{lcw{7,xr{{{get next code word{11083
{{bri{9,(xr){{{execute next code word{11084
{{ejc{{{{{11085
*      vrblk (continued)
*      vrtra (trapped case). this routine is called from the
*      generated code to transfer to a label when a label
*      trace is currently active.
{b_vrt{ent{{{{entry point{11093
{{sub{7,xr{19,*vrtra{{point back to start of vrblk{11094
{{mov{7,xl{7,xr{{copy vrblk pointer{11095
{{mov{8,wa{19,*vrval{{set name offset{11096
{{mov{7,xr{13,vrlbl(xl){{load pointer to trblk{11097
{{bze{3,kvtra{6,bvrt2{{jump if trace is off{11098
{{dcv{3,kvtra{{{else decrement trace count{11099
{{bze{13,trfnc(xr){6,bvrt1{{jump if print trace case{11100
{{jsr{6,trxeq{{{else execute full trace{11101
{{brn{6,bvrt2{{{merge to jump to label{11102
*      here for print trace -- print colon ( label name )
{bvrt1{jsr{6,prtsn{{{print statement number{11106
{{mov{7,xr{7,xl{{copy vrblk pointer{11107
{{mov{8,wa{18,=ch_cl{{colon{11108
{{jsr{6,prtch{{{print it{11109
{{mov{8,wa{18,=ch_pp{{left paren{11110
{{jsr{6,prtch{{{print it{11111
{{jsr{6,prtvn{{{print label name{11112
{{mov{8,wa{18,=ch_rp{{right paren{11113
{{jsr{6,prtch{{{print it{11114
{{jsr{6,prtnl{{{terminate line{11115
{{mov{7,xr{13,vrlbl(xl){{point back to trblk{11116
*      merge here to jump to label
{bvrt2{mov{7,xr{13,trlbl(xr){{load pointer to actual code{11120
{{bri{9,(xr){{{execute statement at label{11121
{{ejc{{{{{11122
*      vrblk (continued)
*      entry for vrsto (trapped case). this routine is called
*      from the generated code to store the value of a variable.
*      this entry is used when a value trace or output
*      association is currently active.
*      (xr)                  pointer to vrsto field of vrblk
{b_vrv{ent{{{{entry point{11133
{{mov{8,wb{9,(xs){{load value (leave copy on stack){11134
{{sub{7,xr{19,*vrsto{{point to vrblk{11135
{{mov{7,xl{7,xr{{copy vrblk pointer{11136
{{mov{8,wa{19,*vrval{{set offset{11137
{{jsr{6,asign{{{call assignment routine{11138
{{ppm{6,exfal{{{fail if assignment fails{11139
{{lcw{7,xr{{{else get next code word{11140
{{bri{9,(xr){{{execute next code word{11141
{{ejc{{{{{11142
*      xnblk
*      the routine for an xnblk is never executed
{b_xnt{ent{2,bl_xn{{{entry point (xnblk){11148
{{ejc{{{{{11149
*      xrblk
*      the routine for an xrblk is never executed
{b_xrt{ent{2,bl_xr{{{entry point (xrblk){11155
*      mark entry address past last block action routine
{b_yyy{ent{2,bl__i{{{last block routine entry point{11159
{{ttl{27,s p i t b o l -- pattern matching routines{{{{11160
*      the following section consists of the pattern matching
*      routines. all pattern nodes contain a pointer (pcode)
*      to one of the routines in this section (p_xxx).
*      note that this section follows the b_xxx routines to
*      enable a fast test for the pattern datatype.
{p_aaa{ent{2,bl__i{{{entry to mark first pattern{11169
*      the entry conditions to the match routine are as follows
*      (see o_pmn, o_pmv, o_pms and procedure match).
*      stack contents.
*                            name base (o_pmn only)
*                            name offset (o_pmn only)
*                            type (0-o_pmn, 1-o_pmv, 2-o_pms)
*      pmhbs --------------- initial cursor (zero)
*                            initial node pointer
*      xs ------------------ =ndabo (anchored), =nduna (unanch)
*      register values.
*           (xs)             set as shown in stack diagram
*           (xr)             pointer to initial pattern node
*           (wb)             initial cursor (zero)
*      global pattern values
*           r_pms            pointer to subject string scblk
*           pmssl            length of subject string in chars
*           pmdfl            dot flag, initially zero
*           pmhbs            set as shown in stack diagram
*      control is passed by branching through the pcode
*      field of the initial pattern node (bri (xr)).
{{ejc{{{{{11199
*      description of algorithm
*      a pattern structure is represented as a linked graph
*      of nodes with the following structure.
*           +------------------------------------+
*           i                pcode               i
*           +------------------------------------+
*           i                pthen               i
*           +------------------------------------+
*           i                parm1               i
*           +------------------------------------+
*           i                parm2               i
*           +------------------------------------+
*      pcode is a pointer to the routine which will perform
*      the match of this particular node type.
*      pthen is a pointer to the successor node. i.e. the node
*      to be matched if the attempt to match this node succeeds.
*      if this is the last node of the pattern pthen points
*      to the dummy node ndnth which initiates pattern exit.
*      parm1, parm2 are parameters whose use varies with the
*      particular node. they are only present if required.
*      alternatives are handled with the special alternative
*      node whose parameter points to the node to be matched
*      if there is a failure on the successor path.
*      the following example illustrates the manner in which
*      the structure is built up. the pattern is
*      (a / b / c) (d / e)   where / is alternation
*      in the diagram, the node marked + represents an
*      alternative node and the dotted line from a + node
*      represents the parameter pointer to the alternative.
*      +---+     +---+     +---+     +---+
*      i + i-----i a i-----i + i-----i d i-----
*      +---+     +---+  i  +---+     +---+
*        .              i    .
*        .              i    .
*      +---+     +---+  i  +---+
*      i + i-----i b i--i  i e i-----
*      +---+     +---+  i  +---+
*        .              i
*        .              i
*      +---+            i
*      i c i------------i
*      +---+
{{ejc{{{{{11253
*      during the match, the registers are used as follows.
*      (xr)                  points to the current node
*      (xl)                  scratch
*      (xs)                  main stack pointer
*      (wb)                  cursor (number of chars matched)
*      (wa,wc)               scratch
*      to keep track of alternatives, the main stack is used as
*      a history stack and contains two word entries.
*      word 1                saved cursor value
*      word 2                node to match on failure
*      when a failure occurs, the most recent entry on this
*      stack is popped off to restore the cursor and point
*      to the node to be matched as an alternative. the entry
*      at the bottom of the stack points to the following
*      special nodes depending on the scan mode.
*      anchored mode         the bottom entry points to the
*                            special node ndabo which causes an
*                            abort. the cursor value stored
*                            with this entry is always zero.
*      unanchored mode       the bottom entry points to the
*                            special node nduna which moves the
*                            anchor point and restarts the match
*                            the cursor saved with this entry
*                            is the number of characters which
*                            lie before the initial anchor point
*                            (i.e. the number of anchor moves).
*                            this entry is three words long and
*                            also contains the initial pattern.
*      entries are made on this history stack by alternative
*      nodes and by some special compound patterns as described
*      later on. the following global locations are used during
*      pattern matching.
*      r_pms                 pointer to subject string
*      pmssl                 length of subject string
*      pmdfl                 flag set non-zero for dot patterns
*      pmhbs                 base ptr for current history stack
*      the following exit points are available to match routines
*      succp                 success in matching current node
*      failp                 failure in matching current node
{{ejc{{{{{11304
*      compound patterns
*      some patterns have implicit alternatives and their
*      representation in the pattern structure consists of a
*      linked set of nodes as indicated by these diagrams.
*      as before, the + represents an alternative node and
*      the dotted line from a + node is the parameter pointer
*      to the alternative pattern.
*      arb
*      ---
*           +---+            this node (p_arb) matches null
*           i b i-----       and stacks cursor, successor ptr,
*           +---+            cursor (copy) and a ptr to ndarc.
*      bal
*      ---
*           +---+            the p_bal node scans a balanced
*           i b i-----       string and then stacks a pointer
*           +---+            to itself on the history stack.
{{ejc{{{{{11332
*      compound pattern structures (continued)
*      arbno
*      -----
*           +---+            this alternative node matches null
*      +----i + i-----       the first time and stacks a pointer
*      i    +---+            to the argument pattern x.
*      i      .
*      i      .
*      i    +---+            node (p_aba) to stack cursor
*      i    i a i            and history stack base ptr.
*      i    +---+
*      i      i
*      i      i
*      i    +---+            this is the argument pattern. as
*      i    i x i            indicated, the successor of the
*      i    +---+            pattern is the p_abc node
*      i      i
*      i      i
*      i    +---+            this node (p_abc) pops pmhbs,
*      +----i c i            stacks old pmhbs and ptr to ndabd
*           +---+            (unless optimization has occurred)
*      structure and execution of this pattern resemble those of
*      recursive pattern matching and immediate assignment.
*      the alternative node at the head of the structure matches
*      null initially but on subsequent failure ensures attempt
*      to match the argument.  before the argument is matched
*      p_aba stacks the cursor, pmhbs and a ptr to p_abb.  if
*      the argument cant be matched , p_abb removes this special
*      stack entry and fails.
*      if argument is matched , p_abc restores the outer pmhbs
*      value (saved by p_aba) .  then if the argument has left
*      alternatives on stack it stacks the inner value of pmhbs
*      and a ptr to ndabd. if argument left nothing on the stack
*      it optimises by removing items stacked by p_aba.  finally
*      a check is made that argument matched more than the null
*      string (check is intended to prevent useless looping).
*      if so the successor is again the alternative node at the
*      head of the structure , ensuring a possible extra attempt
*      to match the arg if necessary.  if not , the successor to
*      alternative is taken so as to terminate the loop.  p_abd
*      restores inner pmhbs ptr and fails , thus trying to match
*      alternatives left by the arbno argument.
{{ejc{{{{{11380
*      compound pattern structures (continued)
*      breakx
*      ------
*           +---+            this node is a break node for
*      +----i b i            the argument to breakx, identical
*      i    +---+            to an ordinary break node.
*      i      i
*      i      i
*      i    +---+            this alternative node stacks a
*      i    i + i-----       pointer to the breakx node to
*      i    +---+            allow for subsequent failure
*      i      .
*      i      .
*      i    +---+            this is the breakx node itself. it
*      +----i x i            matches one character and then
*           +---+            proceeds back to the break node.
*      fence
*      -----
*           +---+            the fence node matches null and
*           i f i-----       stacks a pointer to node ndabo to
*           +---+            abort on a subsequent rematch
*      succeed
*      -------
*           +---+            the node for succeed matches null
*           i s i-----       and stacks a pointer to itself
*           +---+            to repeat the match on a failure.
{{ejc{{{{{11420
*      compound patterns (continued)
*      binary dot (pattern assignment)
*      -------------------------------
*           +---+            this node (p_paa) saves the current
*           i a i            cursor and a pointer to the
*           +---+            special node ndpab on the stack.
*             i
*             i
*           +---+            this is the structure for the
*           i x i            pattern left argument of the
*           +---+            pattern assignment call.
*             i
*             i
*           +---+            this node (p_pac) saves the cursor,
*           i c i-----       a ptr to itself, the cursor (copy)
*           +---+            and a ptr to ndpad on the stack.
*      the function of the match routine for ndpab (p_pab)
*      is simply to unstack itself and fail back onto the stack.
*      the match routine for p_pac also sets the global pattern
*      flag pmdfl non-zero to indicate that pattern assignments
*      may have occured in the pattern match
*      if pmdfl is set at the end of the match (see p_nth), the
*      history stack is scanned for matching ndpab-ndpad pairs
*      and the corresponding pattern assignments are executed.
*      the function of the match routine for ndpad (p_pad)
*      is simply to remove its entry from the stack and fail.
*      this includes removing the special node pointer stored
*      in addition to the standard two entries on the stack.
{{ejc{{{{{11457
*      compount pattern structures (continued)
*      fence (function)
*      ----------------
*           +---+            this node (p_fna) saves the
*           i a i            current history stack and a
*           +---+            pointer to ndfnb on the stack.
*             i
*             i
*           +---+            this is the pattern structure
*           i x i            given as the argument to the
*           +---+            fence function.
*             i
*             i
*           +---+            this node p_fnc restores the outer
*           i c i            history stack ptr saved in p_fna,
*           +---+            and stacks the inner stack base
*                            ptr and a pointer to ndfnd on the
*                            stack.
*      ndfnb (f_fnb) simply is the failure exit for pattern
*      argument failure, and it pops itself and fails onto the
*      stack.
*      the match routine p_fnc allows for an optimization when
*      the fence pattern leaves no alternatives.  in this case,
*      the ndfnb entry is popped, and the match continues.
*      ndfnd (p_fnd) is entered when the pattern fails after
*      going through a non-optimized p_fnc, and it pops the
*      stack back past the innter stack base created by p_fna
{{ejc{{{{{11491
*      compound patterns (continued)
*      expression patterns (recursive pattern matches)
*      -----------------------------------------------
*      initial entry for a pattern node is to the routine p_exa.
*      if the evaluated result of the expression is itself a
*      pattern, then the following steps are taken to arrange
*      for proper recursive processing.
*      1)   a pointer to the current node (the p_exa node) is
*           stored on the history stack with a dummy cursor.
*      2)   a special history stack entry is made in which the
*           node pointer points to ndexb, and the cursor value
*           is the saved value of pmhbs on entry to this node.
*           the match routine for ndexb (p_exb) restores pmhbs
*           from this cursor entry, pops off the p_exa node
*           pointer and fails.
*      3)   the resulting history stack pointer is saved in
*           pmhbs to establish a new level of history stack.
*      after matching a pattern, the end of match routine gets
*      control (p_nth). this routine proceeds as follows.
*      1)   load the current value of pmhbs and recognize the
*           outer level case by the fact that the associated
*           cursor in this case is the pattern match type code
*           which is less than 3. terminate the match in this
*           case and continue execution of the program.
*      2)   otherwise make a special history stack entry in
*           which the node pointer points to the special node
*           ndexc and the cursor is the current value of pmhbs.
*           the match routine for ndexc (p_exc) resets pmhbs to
*           this (inner) value and and then fails.
*      3)   using the history stack entry made on starting the
*           expression (accessible with the current value of
*           pmhbs), restore the p_exa node pointer and the old
*           pmhbs setting. take the successor and continue.
*      an optimization is possible if the expression pattern
*      makes no entries on the history stack. in this case,
*      instead of building the p_exc node in step 2, it is more
*      efficient to simply pop off the p_exb entry and its
*      associated node pointer. the effect is the same.
{{ejc{{{{{11541
*      compound patterns (continued)
*      binary dollar (immediate assignment)
*      ------------------------------------
*           +---+            this node (p_ima) stacks the cursor
*           i a i            pmhbs and a ptr to ndimb and resets
*           +---+            the stack ptr pmhbs.
*             i
*             i
*           +---+            this is the left structure for the
*           i x i            pattern left argument of the
*           +---+            immediate assignment call.
*             i
*             i
*           +---+            this node (p_imc) performs the
*           i c i-----       assignment, pops pmhbs and stacks
*           +---+            the old pmhbs and a ptr to ndimd.
*      the structure and execution of this pattern are similar
*      to those of the recursive expression pattern matching.
*      the match routine for ndimb (p_imb) restores the outer
*      level value of pmhbs, unstacks the saved cursor and fails
*      the match routine p_imc uses the current value of pmhbs
*      to locate the p_imb entry. this entry is used to make
*      the assignment and restore the outer level value of
*      pmhbs. finally, the inner level value of pmhbs and a
*      pointer to the special node ndimd are stacked.
*      the match routine for ndimd (p_imd) restores the inner
*      level value of pmhbs and fails back into the stack.
*      an optimization occurs if the inner pattern makes no
*      entries on the history stack. in this case, p_imc pops
*      the p_imb entry instead of making a p_imd entry.
{{ejc{{{{{11581
*      arbno
*      see compound patterns section for stucture and
*      algorithm for matching this node type.
*      no parameters
{p_aba{ent{2,bl_p0{{{p0blk{11590
{{mov{11,-(xs){8,wb{{stack cursor{11591
{{mov{11,-(xs){7,xr{{stack dummy node ptr{11592
{{mov{11,-(xs){3,pmhbs{{stack old stack base ptr{11593
{{mov{11,-(xs){21,=ndabb{{stack ptr to node ndabb{11594
{{mov{3,pmhbs{7,xs{{store new stack base ptr{11595
{{brn{6,succp{{{succeed{11596
{{ejc{{{{{11597
*      arbno (remove p_aba special stack entry)
*      no parameters (dummy pattern)
{p_abb{ent{{{{entry point{11603
{{mov{3,pmhbs{8,wb{{restore history stack base ptr{11604
{{brn{6,flpop{{{fail and pop dummy node ptr{11605
{{ejc{{{{{11606
*      arbno (check if arg matched null string)
*      no parameters (dummy pattern)
{p_abc{ent{2,bl_p0{{{p0blk{11612
{{mov{7,xt{3,pmhbs{{keep p_abb stack base{11613
{{mov{8,wa{13,num03(xt){{load initial cursor{11614
{{mov{3,pmhbs{13,num01(xt){{restore outer stack base ptr{11615
{{beq{7,xt{7,xs{6,pabc1{jump if no history stack entries{11616
{{mov{11,-(xs){7,xt{{else save inner pmhbs entry{11617
{{mov{11,-(xs){21,=ndabd{{stack ptr to special node ndabd{11618
{{brn{6,pabc2{{{merge{11619
*      optimise case of no extra entries on stack from arbno arg
{pabc1{add{7,xs{19,*num04{{remove ndabb entry and cursor{11623
*      merge to check for matching of null string
{pabc2{bne{8,wa{8,wb{6,succp{allow further attempt if non-null{11627
{{mov{7,xr{13,pthen(xr){{bypass alternative node so as to ...{11628
{{brn{6,succp{{{... refuse further match attempts{11629
{{ejc{{{{{11630
*      arbno (try for alternatives in arbno argument)
*      no parameters (dummy pattern)
{p_abd{ent{{{{entry point{11636
{{mov{3,pmhbs{8,wb{{restore inner stack base ptr{11637
{{brn{6,failp{{{and fail{11638
{{ejc{{{{{11639
*      abort
*      no parameters
{p_abo{ent{2,bl_p0{{{p0blk{11645
{{brn{6,exfal{{{signal statement failure{11646
{{ejc{{{{{11647
*      alternation
*      parm1                 alternative node
{p_alt{ent{2,bl_p1{{{p1blk{11653
{{mov{11,-(xs){8,wb{{stack cursor{11654
{{mov{11,-(xs){13,parm1(xr){{stack pointer to alternative{11655
{{chk{{{{check for stack overflow{11656
{{brn{6,succp{{{if all ok, then succeed{11657
{{ejc{{{{{11658
*      any (one character argument) (1-char string also)
*      parm1                 character argument
{p_ans{ent{2,bl_p1{{{p1blk{11664
{{beq{8,wb{3,pmssl{6,failp{fail if no chars left{11665
{{mov{7,xl{3,r_pms{{else point to subject string{11666
{{plc{7,xl{8,wb{{point to current character{11667
{{lch{8,wa{9,(xl){{load current character{11668
{{bne{8,wa{13,parm1(xr){6,failp{fail if no match{11669
{{icv{8,wb{{{else bump cursor{11670
{{brn{6,succp{{{and succeed{11671
{{ejc{{{{{11672
*      any (multi-character argument case)
*      parm1                 pointer to ctblk
*      parm2                 bit mask to select bit in ctblk
{p_any{ent{2,bl_p2{{{p2blk{11679
*      expression argument case merges here
{pany1{beq{8,wb{3,pmssl{6,failp{fail if no characters left{11683
{{mov{7,xl{3,r_pms{{else point to subject string{11684
{{plc{7,xl{8,wb{{get char ptr to current character{11685
{{lch{8,wa{9,(xl){{load current character{11686
{{mov{7,xl{13,parm1(xr){{point to ctblk{11687
{{wtb{8,wa{{{change to byte offset{11688
{{add{7,xl{8,wa{{point to entry in ctblk{11689
{{mov{8,wa{13,ctchs(xl){{load word from ctblk{11690
{{anb{8,wa{13,parm2(xr){{and with selected bit{11691
{{zrb{8,wa{6,failp{{fail if no match{11692
{{icv{8,wb{{{else bump cursor{11693
{{brn{6,succp{{{and succeed{11694
{{ejc{{{{{11695
*      any (expression argument)
*      parm1                 expression pointer
{p_ayd{ent{2,bl_p1{{{p1blk{11701
{{jsr{6,evals{{{evaluate string argument{11702
{{err{1,043{26,any evaluated argument is not a string{{{11703
{{ppm{6,failp{{{fail if evaluation failure{11704
{{ppm{6,pany1{{{merge multi-char case if ok{11705
{{ejc{{{{{11706
*      p_arb                 initial arb match
*      no parameters
*      the p_arb node is part of a compound pattern structure
*      for an arb pattern (see description of compound patterns)
{p_arb{ent{2,bl_p0{{{p0blk{11715
{{mov{7,xr{13,pthen(xr){{load successor pointer{11716
{{mov{11,-(xs){8,wb{{stack dummy cursor{11717
{{mov{11,-(xs){7,xr{{stack successor pointer{11718
{{mov{11,-(xs){8,wb{{stack cursor{11719
{{mov{11,-(xs){21,=ndarc{{stack ptr to special node ndarc{11720
{{bri{9,(xr){{{execute next node matching null{11721
{{ejc{{{{{11722
*      p_arc                 extend arb match
*      no parameters (dummy pattern)
{p_arc{ent{{{{entry point{11728
{{beq{8,wb{3,pmssl{6,flpop{fail and pop stack to successor{11729
{{icv{8,wb{{{else bump cursor{11730
{{mov{11,-(xs){8,wb{{stack updated cursor{11731
{{mov{11,-(xs){7,xr{{restack pointer to ndarc node{11732
{{mov{7,xr{13,num02(xs){{load successor pointer{11733
{{bri{9,(xr){{{off to reexecute successor node{11734
{{ejc{{{{{11735
*      bal
*      no parameters
*      the p_bal node is part of the compound structure built
*      for bal (see section on compound patterns).
{p_bal{ent{2,bl_p0{{{p0blk{11744
{{zer{8,wc{{{zero parentheses level counter{11745
{{mov{7,xl{3,r_pms{{point to subject string{11746
{{plc{7,xl{8,wb{{point to current character{11747
{{brn{6,pbal2{{{jump into scan loop{11748
*      loop to scan out characters
{pbal1{lch{8,wa{10,(xl)+{{load next character, bump pointer{11752
{{icv{8,wb{{{push cursor for character{11753
{{beq{8,wa{18,=ch_pp{6,pbal3{jump if left paren{11754
{{beq{8,wa{18,=ch_rp{6,pbal4{jump if right paren{11755
{{bze{8,wc{6,pbal5{{else succeed if at outer level{11756
*      here after processing one character
{pbal2{bne{8,wb{3,pmssl{6,pbal1{loop back unless end of string{11760
{{brn{6,failp{{{in which case, fail{11761
*      here on left paren
{pbal3{icv{8,wc{{{bump paren level{11765
{{brn{6,pbal2{{{loop back to check end of string{11766
*      here for right paren
{pbal4{bze{8,wc{6,failp{{fail if no matching left paren{11770
{{dcv{8,wc{{{else decrement level counter{11771
{{bnz{8,wc{6,pbal2{{loop back if not at outer level{11772
*      here after successfully scanning a balanced string
{pbal5{mov{11,-(xs){8,wb{{stack cursor{11776
{{mov{11,-(xs){7,xr{{stack ptr to bal node for extend{11777
{{brn{6,succp{{{and succeed{11778
{{ejc{{{{{11779
*      break (expression argument)
*      parm1                 expression pointer
{p_bkd{ent{2,bl_p1{{{p1blk{11785
{{jsr{6,evals{{{evaluate string expression{11786
{{err{1,044{26,break evaluated argument is not a string{{{11787
{{ppm{6,failp{{{fail if evaluation fails{11788
{{ppm{6,pbrk1{{{merge with multi-char case if ok{11789
{{ejc{{{{{11790
*      break (one character argument)
*      parm1                 character argument
{p_bks{ent{2,bl_p1{{{p1blk{11796
{{mov{8,wc{3,pmssl{{get subject string length{11797
{{sub{8,wc{8,wb{{get number of characters left{11798
{{bze{8,wc{6,failp{{fail if no characters left{11799
{{lct{8,wc{8,wc{{set counter for chars left{11800
{{mov{7,xl{3,r_pms{{point to subject string{11801
{{plc{7,xl{8,wb{{point to current character{11802
*      loop to scan till break character found
{pbks1{lch{8,wa{10,(xl)+{{load next char, bump pointer{11806
{{beq{8,wa{13,parm1(xr){6,succp{succeed if break character found{11807
{{icv{8,wb{{{else push cursor{11808
{{bct{8,wc{6,pbks1{{loop back if more to go{11809
{{brn{6,failp{{{fail if end of string, no break chr{11810
{{ejc{{{{{11811
*      break (multi-character argument)
*      parm1                 pointer to ctblk
*      parm2                 bit mask to select bit column
{p_brk{ent{2,bl_p2{{{p2blk{11818
*      expression argument merges here
{pbrk1{mov{8,wc{3,pmssl{{load subject string length{11822
{{sub{8,wc{8,wb{{get number of characters left{11823
{{bze{8,wc{6,failp{{fail if no characters left{11824
{{lct{8,wc{8,wc{{set counter for characters left{11825
{{mov{7,xl{3,r_pms{{else point to subject string{11826
{{plc{7,xl{8,wb{{point to current character{11827
{{mov{3,psave{7,xr{{save node pointer{11828
*      loop to search for break character
{pbrk2{lch{8,wa{10,(xl)+{{load next char, bump pointer{11832
{{mov{7,xr{13,parm1(xr){{load pointer to ctblk{11833
{{wtb{8,wa{{{convert to byte offset{11834
{{add{7,xr{8,wa{{point to ctblk entry{11835
{{mov{8,wa{13,ctchs(xr){{load ctblk word{11836
{{mov{7,xr{3,psave{{restore node pointer{11837
{{anb{8,wa{13,parm2(xr){{and with selected bit{11838
{{nzb{8,wa{6,succp{{succeed if break character found{11839
{{icv{8,wb{{{else push cursor{11840
{{bct{8,wc{6,pbrk2{{loop back unless end of string{11841
{{brn{6,failp{{{fail if end of string, no break chr{11842
{{ejc{{{{{11843
*      breakx (extension)
*      this is the entry which causes an extension of a breakx
*      match when failure occurs. see section on compound
*      patterns for full details of breakx matching.
*      no parameters
{p_bkx{ent{2,bl_p0{{{p0blk{11853
{{icv{8,wb{{{step cursor past previous break chr{11854
{{brn{6,succp{{{succeed to rematch break{11855
{{ejc{{{{{11856
*      breakx (expression argument)
*      see section on compound patterns for full structure of
*      breakx pattern. the actual character matching uses a
*      break node. however, the entry for the expression
*      argument case is separated to get proper error messages.
*      parm1                 expression pointer
{p_bxd{ent{2,bl_p1{{{p1blk{11867
{{jsr{6,evals{{{evaluate string argument{11868
{{err{1,045{26,breakx evaluated argument is not a string{{{11869
{{ppm{6,failp{{{fail if evaluation fails{11870
{{ppm{6,pbrk1{{{merge with break if all ok{11871
{{ejc{{{{{11872
*      cursor assignment
*      parm1                 name base
*      parm2                 name offset
{p_cas{ent{2,bl_p2{{{p2blk{11879
{{mov{11,-(xs){7,xr{{save node pointer{11880
{{mov{11,-(xs){8,wb{{save cursor{11881
{{mov{7,xl{13,parm1(xr){{load name base{11882
{{mti{8,wb{{{load cursor as integer{11883
{{mov{8,wb{13,parm2(xr){{load name offset{11884
{{jsr{6,icbld{{{get icblk for cursor value{11885
{{mov{8,wa{8,wb{{move name offset{11886
{{mov{8,wb{7,xr{{move value to assign{11887
{{jsr{6,asinp{{{perform assignment{11888
{{ppm{6,flpop{{{fail on assignment failure{11889
{{mov{8,wb{10,(xs)+{{else restore cursor{11890
{{mov{7,xr{10,(xs)+{{restore node pointer{11891
{{brn{6,succp{{{and succeed matching null{11892
{{ejc{{{{{11893
*      expression node (p_exa, initial entry)
*      see compound patterns description for the structure and
*      algorithms for handling expression nodes.
*      parm1                 expression pointer
{p_exa{ent{2,bl_p1{{{p1blk{11902
{{jsr{6,evalp{{{evaluate expression{11903
{{ppm{6,failp{{{fail if evaluation fails{11904
{{blo{8,wa{22,=p_aaa{6,pexa1{jump if result is not a pattern{11905
*      here if result of expression is a pattern
{{mov{11,-(xs){8,wb{{stack dummy cursor{11909
{{mov{11,-(xs){7,xr{{stack ptr to p_exa node{11910
{{mov{11,-(xs){3,pmhbs{{stack history stack base ptr{11911
{{mov{11,-(xs){21,=ndexb{{stack ptr to special node ndexb{11912
{{mov{3,pmhbs{7,xs{{store new stack base pointer{11913
{{mov{7,xr{7,xl{{copy node pointer{11914
{{bri{9,(xr){{{match first node in expression pat{11915
*      here if result of expression is not a pattern
{pexa1{beq{8,wa{22,=b_scl{6,pexa2{jump if it is already a string{11919
{{mov{11,-(xs){7,xl{{else stack result{11920
{{mov{7,xl{7,xr{{save node pointer{11921
{{jsr{6,gtstg{{{convert result to string{11922
{{err{1,046{26,expression does not evaluate to pattern{{{11923
{{mov{8,wc{7,xr{{copy string pointer{11924
{{mov{7,xr{7,xl{{restore node pointer{11925
{{mov{7,xl{8,wc{{copy string pointer again{11926
*      merge here with string pointer in xl
{pexa2{bze{13,sclen(xl){6,succp{{just succeed if null string{11930
{{brn{6,pstr1{{{else merge with string circuit{11931
{{ejc{{{{{11932
*      expression node (p_exb, remove ndexb entry)
*      see compound patterns description for the structure and
*      algorithms for handling expression nodes.
*      no parameters (dummy pattern)
{p_exb{ent{{{{entry point{11941
{{mov{3,pmhbs{8,wb{{restore outer level stack pointer{11942
{{brn{6,flpop{{{fail and pop p_exa node ptr{11943
{{ejc{{{{{11944
*      expression node (p_exc, remove ndexc entry)
*      see compound patterns description for the structure and
*      algorithms for handling expression nodes.
*      no parameters (dummy pattern)
{p_exc{ent{{{{entry point{11953
{{mov{3,pmhbs{8,wb{{restore inner stack base pointer{11954
{{brn{6,failp{{{and fail into expr pattern alternvs{11955
{{ejc{{{{{11956
*      fail
*      no parameters
{p_fal{ent{2,bl_p0{{{p0blk{11962
{{brn{6,failp{{{just signal failure{11963
{{ejc{{{{{11964
*      fence
*      see compound patterns section for the structure and
*      algorithm for matching this node type.
*      no parameters
{p_fen{ent{2,bl_p0{{{p0blk{11973
{{mov{11,-(xs){8,wb{{stack dummy cursor{11974
{{mov{11,-(xs){21,=ndabo{{stack ptr to abort node{11975
{{brn{6,succp{{{and succeed matching null{11976
{{ejc{{{{{11977
*      fence (function)
*      see compound patterns comments at start of this section
*      for details of scheme
*      no parameters
{p_fna{ent{2,bl_p0{{{p0blk{11986
{{mov{11,-(xs){3,pmhbs{{stack current history stack base{11987
{{mov{11,-(xs){21,=ndfnb{{stack indir ptr to p_fnb (failure){11988
{{mov{3,pmhbs{7,xs{{begin new history stack{11989
{{brn{6,succp{{{succeed{11990
{{ejc{{{{{11991
*      fence (function) (reset history stack and fail)
*      no parameters (dummy pattern)
{p_fnb{ent{2,bl_p0{{{p0blk{11997
{{mov{3,pmhbs{8,wb{{restore outer pmhbs stack base{11998
{{brn{6,failp{{{...and fail{11999
{{ejc{{{{{12000
*      fence (function) (make fence trap entry on stack)
*      no parameters (dummy pattern)
{p_fnc{ent{2,bl_p0{{{p0blk{12006
{{mov{7,xt{3,pmhbs{{get inner stack base ptr{12007
{{mov{3,pmhbs{13,num01(xt){{restore outer stack base{12008
{{beq{7,xt{7,xs{6,pfnc1{optimize if no alternatives{12009
{{mov{11,-(xs){7,xt{{else stack inner stack base{12010
{{mov{11,-(xs){21,=ndfnd{{stack ptr to ndfnd{12011
{{brn{6,succp{{{succeed{12012
*      here when fence function left nothing on the stack
{pfnc1{add{7,xs{19,*num02{{pop off p_fnb entry{12016
{{brn{6,succp{{{succeed{12017
{{ejc{{{{{12018
*      fence (function) (skip past alternatives on failure)
*      no parameters (dummy pattern)
{p_fnd{ent{2,bl_p0{{{p0blk{12024
{{mov{7,xs{8,wb{{pop stack to fence() history base{12025
{{brn{6,flpop{{{pop base entry and fail{12026
{{ejc{{{{{12027
*      immediate assignment (initial entry, save current cursor)
*      see compound patterns description for details of the
*      structure and algorithm for matching this node type.
*      no parameters
{p_ima{ent{2,bl_p0{{{p0blk{12036
{{mov{11,-(xs){8,wb{{stack cursor{12037
{{mov{11,-(xs){7,xr{{stack dummy node pointer{12038
{{mov{11,-(xs){3,pmhbs{{stack old stack base pointer{12039
{{mov{11,-(xs){21,=ndimb{{stack ptr to special node ndimb{12040
{{mov{3,pmhbs{7,xs{{store new stack base pointer{12041
{{brn{6,succp{{{and succeed{12042
{{ejc{{{{{12043
*      immediate assignment (remove cursor mark entry)
*      see compound patterns description for details of the
*      structure and algorithms for matching this node type.
*      no parameters (dummy pattern)
{p_imb{ent{{{{entry point{12052
{{mov{3,pmhbs{8,wb{{restore history stack base ptr{12053
{{brn{6,flpop{{{fail and pop dummy node ptr{12054
{{ejc{{{{{12055
*      immediate assignment (perform actual assignment)
*      see compound patterns description for details of the
*      structure and algorithms for matching this node type.
*      parm1                 name base of variable
*      parm2                 name offset of variable
{p_imc{ent{2,bl_p2{{{p2blk{12065
{{mov{7,xt{3,pmhbs{{load pointer to p_imb entry{12066
{{mov{8,wa{8,wb{{copy final cursor{12067
{{mov{8,wb{13,num03(xt){{load initial cursor{12068
{{mov{3,pmhbs{13,num01(xt){{restore outer stack base pointer{12069
{{beq{7,xt{7,xs{6,pimc1{jump if no history stack entries{12070
{{mov{11,-(xs){7,xt{{else save inner pmhbs pointer{12071
{{mov{11,-(xs){21,=ndimd{{and a ptr to special node ndimd{12072
{{brn{6,pimc2{{{merge{12073
*      here if no entries made on history stack
{pimc1{add{7,xs{19,*num04{{remove ndimb entry and cursor{12077
*      merge here to perform assignment
{pimc2{mov{11,-(xs){8,wa{{save current (final) cursor{12081
{{mov{11,-(xs){7,xr{{save current node pointer{12082
{{mov{7,xl{3,r_pms{{point to subject string{12083
{{sub{8,wa{8,wb{{compute substring length{12084
{{jsr{6,sbstr{{{build substring{12085
{{mov{8,wb{7,xr{{move result{12086
{{mov{7,xr{9,(xs){{reload node pointer{12087
{{mov{7,xl{13,parm1(xr){{load name base{12088
{{mov{8,wa{13,parm2(xr){{load name offset{12089
{{jsr{6,asinp{{{perform assignment{12090
{{ppm{6,flpop{{{fail if assignment fails{12091
{{mov{7,xr{10,(xs)+{{else restore node pointer{12092
{{mov{8,wb{10,(xs)+{{restore cursor{12093
{{brn{6,succp{{{and succeed{12094
{{ejc{{{{{12095
*      immediate assignment (remove ndimd entry on failure)
*      see compound patterns description for details of the
*      structure and algorithms for matching this node type.
*      no parameters (dummy pattern)
{p_imd{ent{{{{entry point{12104
{{mov{3,pmhbs{8,wb{{restore inner stack base pointer{12105
{{brn{6,failp{{{and fail{12106
{{ejc{{{{{12107
*      len (integer argument)
*      parm1                 integer argument
{p_len{ent{2,bl_p1{{{p1blk{12113
*      expression argument case merges here
{plen1{add{8,wb{13,parm1(xr){{push cursor indicated amount{12117
{{ble{8,wb{3,pmssl{6,succp{succeed if not off end{12118
{{brn{6,failp{{{else fail{12119
{{ejc{{{{{12120
*      len (expression argument)
*      parm1                 expression pointer
{p_lnd{ent{2,bl_p1{{{p1blk{12126
{{jsr{6,evali{{{evaluate integer argument{12127
{{err{1,047{26,len evaluated argument is not integer{{{12128
{{err{1,048{26,len evaluated argument is negative or too large{{{12129
{{ppm{6,failp{{{fail if evaluation fails{12130
{{ppm{6,plen1{{{merge with normal circuit if ok{12131
{{ejc{{{{{12132
*      notany (expression argument)
*      parm1                 expression pointer
{p_nad{ent{2,bl_p1{{{p1blk{12138
{{jsr{6,evals{{{evaluate string argument{12139
{{err{1,049{26,notany evaluated argument is not a string{{{12140
{{ppm{6,failp{{{fail if evaluation fails{12141
{{ppm{6,pnay1{{{merge with multi-char case if ok{12142
{{ejc{{{{{12143
*      notany (one character argument)
*      parm1                 character argument
{p_nas{ent{2,bl_p1{{{entry point{12149
{{beq{8,wb{3,pmssl{6,failp{fail if no chars left{12150
{{mov{7,xl{3,r_pms{{else point to subject string{12151
{{plc{7,xl{8,wb{{point to current character in strin{12152
{{lch{8,wa{9,(xl){{load current character{12153
{{beq{8,wa{13,parm1(xr){6,failp{fail if match{12154
{{icv{8,wb{{{else bump cursor{12155
{{brn{6,succp{{{and succeed{12156
{{ejc{{{{{12157
*      notany (multi-character string argument)
*      parm1                 pointer to ctblk
*      parm2                 bit mask to select bit column
{p_nay{ent{2,bl_p2{{{p2blk{12164
*      expression argument case merges here
{pnay1{beq{8,wb{3,pmssl{6,failp{fail if no characters left{12168
{{mov{7,xl{3,r_pms{{else point to subject string{12169
{{plc{7,xl{8,wb{{point to current character{12170
{{lch{8,wa{9,(xl){{load current character{12171
{{wtb{8,wa{{{convert to byte offset{12172
{{mov{7,xl{13,parm1(xr){{load pointer to ctblk{12173
{{add{7,xl{8,wa{{point to entry in ctblk{12174
{{mov{8,wa{13,ctchs(xl){{load entry from ctblk{12175
{{anb{8,wa{13,parm2(xr){{and with selected bit{12176
{{nzb{8,wa{6,failp{{fail if character is matched{12177
{{icv{8,wb{{{else bump cursor{12178
{{brn{6,succp{{{and succeed{12179
{{ejc{{{{{12180
*      end of pattern match
*      this routine is entered on successful completion.
*      see description of expression patterns in compound
*      pattern section for handling of recursion in matching.
*      this pattern also results from an attempt to convert the
*      null string to a pattern via convert()
*      no parameters (dummy pattern)
{p_nth{ent{2,bl_p0{{{p0blk (dummy){12193
{{mov{7,xt{3,pmhbs{{load pointer to base of stack{12194
{{mov{8,wa{13,num01(xt){{load saved pmhbs (or pattern type){12195
{{ble{8,wa{18,=num02{6,pnth2{jump if outer level (pattern type){12196
*      here we are at the end of matching an expression pattern
{{mov{3,pmhbs{8,wa{{restore outer stack base pointer{12200
{{mov{7,xr{13,num02(xt){{restore pointer to p_exa node{12201
{{beq{7,xt{7,xs{6,pnth1{jump if no history stack entries{12202
{{mov{11,-(xs){7,xt{{else stack inner stack base ptr{12203
{{mov{11,-(xs){21,=ndexc{{stack ptr to special node ndexc{12204
{{brn{6,succp{{{and succeed{12205
*      here if no history stack entries during pattern
{pnth1{add{7,xs{19,*num04{{remove p_exb entry and node ptr{12209
{{brn{6,succp{{{and succeed{12210
*      here if end of match at outer level
{pnth2{mov{3,pmssl{8,wb{{save final cursor in safe place{12214
{{bze{3,pmdfl{6,pnth6{{jump if no pattern assignments{12215
{{ejc{{{{{12216
*      end of pattern match (continued)
*      now we must perform pattern assignments. this is done by
*      scanning the history stack for matching ndpab-ndpad pairs
{pnth3{dca{7,xt{{{point past cursor entry{12223
{{mov{8,wa{11,-(xt){{load node pointer{12224
{{beq{8,wa{21,=ndpad{6,pnth4{jump if ndpad entry{12225
{{bne{8,wa{21,=ndpab{6,pnth5{jump if not ndpab entry{12226
*      here for ndpab entry, stack initial cursor
*      note that there must be more entries on the stack.
{{mov{11,-(xs){13,num01(xt){{stack initial cursor{12231
{{chk{{{{check for stack overflow{12232
{{brn{6,pnth3{{{loop back if ok{12233
*      here for ndpad entry. the starting cursor from the
*      matching ndpad entry is now the top stack entry.
{pnth4{mov{8,wa{13,num01(xt){{load final cursor{12238
{{mov{8,wb{9,(xs){{load initial cursor from stack{12239
{{mov{9,(xs){7,xt{{save history stack scan ptr{12240
{{sub{8,wa{8,wb{{compute length of string{12241
*      build substring and perform assignment
{{mov{7,xl{3,r_pms{{point to subject string{12245
{{jsr{6,sbstr{{{construct substring{12246
{{mov{8,wb{7,xr{{copy substring pointer{12247
{{mov{7,xt{9,(xs){{reload history stack scan ptr{12248
{{mov{7,xl{13,num02(xt){{load pointer to p_pac node with nam{12249
{{mov{8,wa{13,parm2(xl){{load name offset{12250
{{mov{7,xl{13,parm1(xl){{load name base{12251
{{jsr{6,asinp{{{perform assignment{12252
{{ppm{6,exfal{{{match fails if name eval fails{12253
{{mov{7,xt{10,(xs)+{{else restore history stack ptr{12254
{{ejc{{{{{12255
*      end of pattern match (continued)
*      here check for end of entries
{pnth5{bne{7,xt{7,xs{6,pnth3{loop if more entries to scan{12261
*      here after dealing with pattern assignments
{pnth6{mov{7,xs{3,pmhbs{{wipe out history stack{12265
{{mov{8,wb{10,(xs)+{{load initial cursor{12266
{{mov{8,wc{10,(xs)+{{load match type code{12267
{{mov{8,wa{3,pmssl{{load final cursor value{12268
{{mov{7,xl{3,r_pms{{point to subject string{12269
{{zer{3,r_pms{{{clear subject string ptr for gbcol{12270
{{bze{8,wc{6,pnth7{{jump if call by name{12271
{{beq{8,wc{18,=num02{6,pnth9{exit if statement level call{12272
*      here we have a call by value, build substring
{{sub{8,wa{8,wb{{compute length of string{12276
{{jsr{6,sbstr{{{build substring{12277
{{mov{11,-(xs){7,xr{{stack result{12278
{{lcw{7,xr{{{get next code word{12279
{{bri{9,(xr){{{execute it{12280
*      here for call by name, make stack entries for o_rpl
{pnth7{mov{11,-(xs){8,wb{{stack initial cursor{12284
{{mov{11,-(xs){8,wa{{stack final cursor{12285
*      here with xl pointing to scblk or bcblk
{pnth8{mov{11,-(xs){7,xl{{stack subject pointer{12294
*      here to obey next code word
{pnth9{lcw{7,xr{{{get next code word{12298
{{bri{9,(xr){{{execute next code word{12299
{{ejc{{{{{12300
*      pos (integer argument)
*      parm1                 integer argument
{p_pos{ent{2,bl_p1{{{p1blk{12306
*      optimize pos if it is the first pattern element,
*      unanchored mode, cursor is zero and pos argument
*      is not beyond end of string.  force cursor position
*      and number of unanchored moves.
*      this optimization is performed invisible provided
*      the argument is either a simple integer or an
*      expression that is an untraced variable (that is,
*      it has no side effects that would be lost by short-
*      circuiting the normal logic of failing and moving the
*      unanchored starting point.)
*      pos (integer argument)
*      parm1                 integer argument
{{beq{8,wb{13,parm1(xr){6,succp{succeed if at right location{12324
{{bnz{8,wb{6,failp{{don't look further if cursor not 0{12325
{{mov{7,xt{3,pmhbs{{get history stack base ptr{12326
{{bne{7,xr{11,-(xt){6,failp{fail if pos is not first node{12327
*      expression argument circuit merges here
{ppos2{bne{11,-(xt){21,=nduna{6,failp{fail if not unanchored mode{12331
{{mov{8,wb{13,parm1(xr){{get desired cursor position{12332
{{bgt{8,wb{3,pmssl{6,exfal{abort if off end{12333
{{mov{13,num02(xt){8,wb{{fake number of unanchored moves{12334
{{brn{6,succp{{{continue match with adjusted cursor{12335
{{ejc{{{{{12336
*      pos (expression argument)
*      parm1                 expression pointer
{p_psd{ent{2,bl_p1{{{p1blk{12342
{{jsr{6,evali{{{evaluate integer argument{12343
{{err{1,050{26,pos evaluated argument is not integer{{{12344
{{err{1,051{26,pos evaluated argument is negative or too large{{{12345
{{ppm{6,failp{{{fail if evaluation fails{12346
{{ppm{6,ppos1{{{process expression case{12347
{ppos1{beq{8,wb{13,parm1(xr){6,succp{succeed if at right location{12349
{{bnz{8,wb{6,failp{{don't look further if cursor not 0{12350
{{bnz{3,evlif{6,failp{{fail if complex argument{12351
{{mov{7,xt{3,pmhbs{{get history stack base ptr{12352
{{mov{8,wa{3,evlio{{get original node ptr{12353
{{bne{8,wa{11,-(xt){6,failp{fail if pos is not first node{12354
{{brn{6,ppos2{{{merge with integer argument code{12355
{{ejc{{{{{12356
*      pattern assignment (initial entry, save cursor)
*      see compound patterns description for the structure and
*      algorithms for matching this node type.
*      no parameters
{p_paa{ent{2,bl_p0{{{p0blk{12365
{{mov{11,-(xs){8,wb{{stack initial cursor{12366
{{mov{11,-(xs){21,=ndpab{{stack ptr to ndpab special node{12367
{{brn{6,succp{{{and succeed matching null{12368
{{ejc{{{{{12369
*      pattern assignment (remove saved cursor)
*      see compound patterns description for the structure and
*      algorithms for matching this node type.
*      no parameters (dummy pattern)
{p_pab{ent{{{{entry point{12378
{{brn{6,failp{{{just fail (entry is already popped){12379
{{ejc{{{{{12380
*      pattern assignment (end of match, make assign entry)
*      see compound patterns description for the structure and
*      algorithms for matching this node type.
*      parm1                 name base of variable
*      parm2                 name offset of variable
{p_pac{ent{2,bl_p2{{{p2blk{12390
{{mov{11,-(xs){8,wb{{stack dummy cursor value{12391
{{mov{11,-(xs){7,xr{{stack pointer to p_pac node{12392
{{mov{11,-(xs){8,wb{{stack final cursor{12393
{{mov{11,-(xs){21,=ndpad{{stack ptr to special ndpad node{12394
{{mnz{3,pmdfl{{{set dot flag non-zero{12395
{{brn{6,succp{{{and succeed{12396
{{ejc{{{{{12397
*      pattern assignment (remove assign entry)
*      see compound patterns description for the structure and
*      algorithms for matching this node type.
*      no parameters (dummy node)
{p_pad{ent{{{{entry point{12406
{{brn{6,flpop{{{fail and remove p_pac node{12407
{{ejc{{{{{12408
*      rem
*      no parameters
{p_rem{ent{2,bl_p0{{{p0blk{12414
{{mov{8,wb{3,pmssl{{point cursor to end of string{12415
{{brn{6,succp{{{and succeed{12416
{{ejc{{{{{12417
*      rpos (expression argument)
*      optimize rpos if it is the first pattern element,
*      unanchored mode, cursor is zero and rpos argument
*      is not beyond end of string.  force cursor position
*      and number of unanchored moves.
*      this optimization is performed invisibly provided
*      the argument is either a simple integer or an
*      expression that is an untraced variable (that is,
*      it has no side effects that would be lost by short-
*      circuiting the normal logic of failing and moving the
*      unanchored starting point).
*      parm1                 expression pointer
{p_rpd{ent{2,bl_p1{{{p1blk{12435
{{jsr{6,evali{{{evaluate integer argument{12436
{{err{1,052{26,rpos evaluated argument is not integer{{{12437
{{err{1,053{26,rpos evaluated argument is negative or too large{{{12438
{{ppm{6,failp{{{fail if evaluation fails{12439
{{ppm{6,prps1{{{merge with normal case if ok{12440
{prps1{mov{8,wc{3,pmssl{{get length of string{12442
{{sub{8,wc{8,wb{{get number of characters remaining{12443
{{beq{8,wc{13,parm1(xr){6,succp{succeed if at right location{12444
{{bnz{8,wb{6,failp{{don't look further if cursor not 0{12445
{{bnz{3,evlif{6,failp{{fail if complex argument{12446
{{mov{7,xt{3,pmhbs{{get history stack base ptr{12447
{{mov{8,wa{3,evlio{{get original node ptr{12448
{{bne{8,wa{11,-(xt){6,failp{fail if pos is not first node{12449
{{brn{6,prps2{{{merge with integer arg code{12450
{{ejc{{{{{12451
*      rpos (integer argument)
*      parm1                 integer argument
{p_rps{ent{2,bl_p1{{{p1blk{12457
*      rpos (integer argument)
*      parm1                 integer argument
{{mov{8,wc{3,pmssl{{get length of string{12463
{{sub{8,wc{8,wb{{get number of characters remaining{12464
{{beq{8,wc{13,parm1(xr){6,succp{succeed if at right location{12465
{{bnz{8,wb{6,failp{{don't look further if cursor not 0{12466
{{mov{7,xt{3,pmhbs{{get history stack base ptr{12467
{{bne{7,xr{11,-(xt){6,failp{fail if rpos is not first node{12468
*      expression argument merges here
{prps2{bne{11,-(xt){21,=nduna{6,failp{fail if not unanchored mode{12472
{{mov{8,wb{3,pmssl{{point to end of string{12473
{{blt{8,wb{13,parm1(xr){6,failp{fail if string not long enough{12474
{{sub{8,wb{13,parm1(xr){{else set new cursor{12475
{{mov{13,num02(xt){8,wb{{fake number of unanchored moves{12476
{{brn{6,succp{{{continue match with adjusted cursor{12477
{{ejc{{{{{12478
*      rtab (integer argument)
*      parm1                 integer argument
{p_rtb{ent{2,bl_p1{{{p1blk{12484
*      expression argument case merges here
{prtb1{mov{8,wc{8,wb{{save initial cursor{12488
{{mov{8,wb{3,pmssl{{point to end of string{12489
{{blt{8,wb{13,parm1(xr){6,failp{fail if string not long enough{12490
{{sub{8,wb{13,parm1(xr){{else set new cursor{12491
{{bge{8,wb{8,wc{6,succp{and succeed if not too far already{12492
{{brn{6,failp{{{in which case, fail{12493
{{ejc{{{{{12494
*      rtab (expression argument)
*      parm1                 expression pointer
{p_rtd{ent{2,bl_p1{{{p1blk{12500
{{jsr{6,evali{{{evaluate integer argument{12501
{{err{1,054{26,rtab evaluated argument is not integer{{{12502
{{err{1,055{26,rtab evaluated argument is negative or too large{{{12503
{{ppm{6,failp{{{fail if evaluation fails{12504
{{ppm{6,prtb1{{{merge with normal case if success{12505
{{ejc{{{{{12506
*      span (expression argument)
*      parm1                 expression pointer
{p_spd{ent{2,bl_p1{{{p1blk{12512
{{jsr{6,evals{{{evaluate string argument{12513
{{err{1,056{26,span evaluated argument is not a string{{{12514
{{ppm{6,failp{{{fail if evaluation fails{12515
{{ppm{6,pspn1{{{merge with multi-char case if ok{12516
{{ejc{{{{{12517
*      span (multi-character argument case)
*      parm1                 pointer to ctblk
*      parm2                 bit mask to select bit column
{p_spn{ent{2,bl_p2{{{p2blk{12524
*      expression argument case merges here
{pspn1{mov{8,wc{3,pmssl{{copy subject string length{12528
{{sub{8,wc{8,wb{{calculate number of characters left{12529
{{bze{8,wc{6,failp{{fail if no characters left{12530
{{mov{7,xl{3,r_pms{{point to subject string{12531
{{plc{7,xl{8,wb{{point to current character{12532
{{mov{3,psavc{8,wb{{save initial cursor{12533
{{mov{3,psave{7,xr{{save node pointer{12534
{{lct{8,wc{8,wc{{set counter for chars left{12535
*      loop to scan matching characters
{pspn2{lch{8,wa{10,(xl)+{{load next character, bump pointer{12539
{{wtb{8,wa{{{convert to byte offset{12540
{{mov{7,xr{13,parm1(xr){{point to ctblk{12541
{{add{7,xr{8,wa{{point to ctblk entry{12542
{{mov{8,wa{13,ctchs(xr){{load ctblk entry{12543
{{mov{7,xr{3,psave{{restore node pointer{12544
{{anb{8,wa{13,parm2(xr){{and with selected bit{12545
{{zrb{8,wa{6,pspn3{{jump if no match{12546
{{icv{8,wb{{{else push cursor{12547
{{bct{8,wc{6,pspn2{{loop back unless end of string{12548
*      here after scanning matching characters
{pspn3{bne{8,wb{3,psavc{6,succp{succeed if chars matched{12552
{{brn{6,failp{{{else fail if null string matched{12553
{{ejc{{{{{12554
*      span (one character argument)
*      parm1                 character argument
{p_sps{ent{2,bl_p1{{{p1blk{12560
{{mov{8,wc{3,pmssl{{get subject string length{12561
{{sub{8,wc{8,wb{{calculate number of characters left{12562
{{bze{8,wc{6,failp{{fail if no characters left{12563
{{mov{7,xl{3,r_pms{{else point to subject string{12564
{{plc{7,xl{8,wb{{point to current character{12565
{{mov{3,psavc{8,wb{{save initial cursor{12566
{{lct{8,wc{8,wc{{set counter for characters left{12567
*      loop to scan matching characters
{psps1{lch{8,wa{10,(xl)+{{load next character, bump pointer{12571
{{bne{8,wa{13,parm1(xr){6,psps2{jump if no match{12572
{{icv{8,wb{{{else push cursor{12573
{{bct{8,wc{6,psps1{{and loop unless end of string{12574
*      here after scanning matching characters
{psps2{bne{8,wb{3,psavc{6,succp{succeed if chars matched{12578
{{brn{6,failp{{{fail if null string matched{12579
{{ejc{{{{{12580
*      multi-character string
*      note that one character strings use the circuit for
*      one character any arguments (p_an1).
*      parm1                 pointer to scblk for string arg
{p_str{ent{2,bl_p1{{{p1blk{12589
{{mov{7,xl{13,parm1(xr){{get pointer to string{12590
*      merge here after evaluating expression with string value
{pstr1{mov{3,psave{7,xr{{save node pointer{12594
{{mov{7,xr{3,r_pms{{load subject string pointer{12595
{{plc{7,xr{8,wb{{point to current character{12596
{{add{8,wb{13,sclen(xl){{compute new cursor position{12597
{{bgt{8,wb{3,pmssl{6,failp{fail if past end of string{12598
{{mov{3,psavc{8,wb{{save updated cursor{12599
{{mov{8,wa{13,sclen(xl){{get number of chars to compare{12600
{{plc{7,xl{{{point to chars of test string{12601
{{cmc{6,failp{6,failp{{compare, fail if not equal{12602
{{mov{7,xr{3,psave{{if all matched, restore node ptr{12603
{{mov{8,wb{3,psavc{{restore updated cursor{12604
{{brn{6,succp{{{and succeed{12605
{{ejc{{{{{12606
*      succeed
*      see section on compound patterns for details of the
*      structure and algorithms for matching this node type
*      no parameters
{p_suc{ent{2,bl_p0{{{p0blk{12615
{{mov{11,-(xs){8,wb{{stack cursor{12616
{{mov{11,-(xs){7,xr{{stack pointer to this node{12617
{{brn{6,succp{{{succeed matching null{12618
{{ejc{{{{{12619
*      tab (integer argument)
*      parm1                 integer argument
{p_tab{ent{2,bl_p1{{{p1blk{12625
*      expression argument case merges here
{ptab1{bgt{8,wb{13,parm1(xr){6,failp{fail if too far already{12629
{{mov{8,wb{13,parm1(xr){{else set new cursor position{12630
{{ble{8,wb{3,pmssl{6,succp{succeed if not off end{12631
{{brn{6,failp{{{else fail{12632
{{ejc{{{{{12633
*      tab (expression argument)
*      parm1                 expression pointer
{p_tbd{ent{2,bl_p1{{{p1blk{12639
{{jsr{6,evali{{{evaluate integer argument{12640
{{err{1,057{26,tab evaluated argument is not integer{{{12641
{{err{1,058{26,tab evaluated argument is negative or too large{{{12642
{{ppm{6,failp{{{fail if evaluation fails{12643
{{ppm{6,ptab1{{{merge with normal case if ok{12644
{{ejc{{{{{12645
*      anchor movement
*      no parameters (dummy node)
{p_una{ent{{{{entry point{12651
{{mov{7,xr{8,wb{{copy initial pattern node pointer{12652
{{mov{8,wb{9,(xs){{get initial cursor{12653
{{beq{8,wb{3,pmssl{6,exfal{match fails if at end of string{12654
{{icv{8,wb{{{else increment cursor{12655
{{mov{9,(xs){8,wb{{store incremented cursor{12656
{{mov{11,-(xs){7,xr{{restack initial node ptr{12657
{{mov{11,-(xs){21,=nduna{{restack unanchored node{12658
{{bri{9,(xr){{{rematch first node{12659
{{ejc{{{{{12660
*      end of pattern match routines
*      the following entry point marks the end of the pattern
*      matching routines and also the end of the entry points
*      referenced from the first word of blocks in dynamic store
{p_yyy{ent{2,bl__i{{{mark last entry in pattern section{12668
{{ttl{27,s p i t b o l -- snobol4 built-in label routines{{{{12669
*      the following section contains the routines for labels
*      which have a predefined meaning in snobol4.
*      control is passed directly to the label name entry point.
*      entry names are of the form l_xxx where xxx is the three
*      letter variable name identifier.
*      entries are in alphabetical order
{{ejc{{{{{12680
*      abort
{l_abo{ent{{{{entry point{12684
*      merge here if execution terminates in error
{labo1{mov{8,wa{3,kvert{{load error code{12688
{{bze{8,wa{6,labo3{{jump if no error has occured{12689
{{jsr{6,sysax{{{call after execution proc{12691
{{mov{8,wc{3,kvstn{{current statement{12695
{{jsr{6,filnm{{{obtain file name for this statement{12696
{{mov{7,xr{3,r_cod{{current code block{12699
{{mov{8,wc{13,cdsln(xr){{line number{12700
{{zer{8,wb{{{column number{12704
{{mov{7,xr{3,stage{{{12705
{{jsr{6,sysea{{{advise system of error{12706
{{ppm{6,stpr4{{{if system does not want print{12707
{{jsr{6,prtpg{{{else eject printer{12709
{{bze{7,xr{6,labo2{{did sysea request print{12711
{{jsr{6,prtst{{{print text from sysea{12712
{labo2{jsr{6,ermsg{{{print error message{12714
{{zer{7,xr{{{indicate no message to print{12715
{{brn{6,stopr{{{jump to routine to stop run{12716
*      here if no error had occured
{labo3{erb{1,036{26,goto abort with no preceding error{{{12720
{{ejc{{{{{12721
*      continue
{l_cnt{ent{{{{entry point{12725
*      merge here after execution error
{lcnt1{mov{7,xr{3,r_cnt{{load continuation code block ptr{12729
{{bze{7,xr{6,lcnt3{{jump if no previous error{12730
{{zer{3,r_cnt{{{clear flag{12731
{{mov{3,r_cod{7,xr{{else store as new code block ptr{12732
{{bne{9,(xr){22,=b_cdc{6,lcnt2{jump if not complex go{12733
{{mov{8,wa{3,stxoc{{get offset of error{12734
{{bge{8,wa{3,stxof{6,lcnt4{jump if error in goto evaluation{12735
*      here if error did not occur in complex failure goto
{lcnt2{add{7,xr{3,stxof{{add failure offset{12739
{{lcp{7,xr{{{load code pointer{12740
{{mov{7,xs{3,flptr{{reset stack pointer{12741
{{lcw{7,xr{{{get next code word{12742
{{bri{9,(xr){{{execute next code word{12743
*      here if no previous error
{lcnt3{icv{3,errft{{{fatal error{12747
{{erb{1,037{26,goto continue with no preceding error{{{12748
*      here if error in evaluation of failure goto.
*      cannot continue back to failure goto!
{lcnt4{icv{3,errft{{{fatal error{12753
{{erb{1,332{26,goto continue with error in failure goto{{{12754
{{ejc{{{{{12755
*      end
{l_end{ent{{{{entry point{12759
*      merge here from end code circuit
{lend0{mov{7,xr{21,=endms{{point to message /normal term.../{12763
{{brn{6,stopr{{{jump to routine to stop run{12764
{{ejc{{{{{12765
*      freturn
{l_frt{ent{{{{entry point{12769
{{mov{8,wa{21,=scfrt{{point to string /freturn/{12770
{{brn{6,retrn{{{jump to common return routine{12771
{{ejc{{{{{12772
*      nreturn
{l_nrt{ent{{{{entry point{12776
{{mov{8,wa{21,=scnrt{{point to string /nreturn/{12777
{{brn{6,retrn{{{jump to common return routine{12778
{{ejc{{{{{12779
*      return
{l_rtn{ent{{{{entry point{12783
{{mov{8,wa{21,=scrtn{{point to string /return/{12784
{{brn{6,retrn{{{jump to common return routine{12785
{{ejc{{{{{12786
*      scontinue
{l_scn{ent{{{{entry point{12790
{{mov{7,xr{3,r_cnt{{load continuation code block ptr{12791
{{bze{7,xr{6,lscn2{{jump if no previous error{12792
{{zer{3,r_cnt{{{clear flag{12793
{{bne{3,kvert{18,=nm320{6,lscn1{error must be user interrupt{12794
{{beq{3,kvert{18,=nm321{6,lscn2{detect scontinue loop{12795
{{mov{3,r_cod{7,xr{{else store as new code block ptr{12796
{{add{7,xr{3,stxoc{{add resume offset{12797
{{lcp{7,xr{{{load code pointer{12798
{{lcw{7,xr{{{get next code word{12799
{{bri{9,(xr){{{execute next code word{12800
*      here if no user interrupt
{lscn1{icv{3,errft{{{fatal error{12804
{{erb{1,331{26,goto scontinue with no user interrupt{{{12805
*      here if in scontinue loop or if no previous error
{lscn2{icv{3,errft{{{fatal error{12809
{{erb{1,321{26,goto scontinue with no preceding error{{{12810
{{ejc{{{{{12811
*      undefined label
{l_und{ent{{{{entry point{12815
{{erb{1,038{26,goto undefined label{{{12816
{{ttl{27,s p i t b o l -- predefined snobol4 functions{{{{12817
*      the following section contains coding for functions
*      which are predefined and available at the snobol level.
*      these routines receive control directly from the code or
*      indirectly through the o_fnc, o_fns or cfunc routines.
*      in both cases the conditions on entry are as follows
*      the arguments are on the stack. the number of arguments
*      has been adjusted to correspond to the svblk svnar field.
*      in certain functions the direct call is not permitted
*      and in these instances we also have.
*      (wa)                  actual number of arguments in call
*      control returns by placing the function result value on
*      on the stack and continuing execution with the next
*      word from the generated code.
*      the names of the entry points of these functions are of
*      the form s_xxx where xxx is the three letter code for
*      the system variable name. the functions are in order
*      alphabetically by their entry names.
{{ejc{{{{{12842
*      any
{s_any{ent{{{{entry point{12896
{{mov{8,wb{22,=p_ans{{set pcode for single char case{12897
{{mov{7,xl{22,=p_any{{pcode for multi-char case{12898
{{mov{8,wc{22,=p_ayd{{pcode for expression case{12899
{{jsr{6,patst{{{call common routine to build node{12900
{{err{1,059{26,any argument is not a string or expression{{{12901
{{mov{11,-(xs){7,xr{{stack result{12902
{{lcw{7,xr{{{get next code word{12903
{{bri{9,(xr){{{execute it{12904
{{ejc{{{{{12905
*      apply
*      apply does not permit the direct (fast) call so that
*      wa contains the actual number of arguments passed.
{s_app{ent{{{{entry point{12931
{{bze{8,wa{6,sapp3{{jump if no arguments{12932
{{dcv{8,wa{{{else get applied func arg count{12933
{{mov{8,wb{8,wa{{copy{12934
{{wtb{8,wb{{{convert to bytes{12935
{{mov{7,xt{7,xs{{copy stack pointer{12936
{{add{7,xt{8,wb{{point to function argument on stack{12937
{{mov{7,xr{9,(xt){{load function ptr (apply 1st arg){12938
{{bze{8,wa{6,sapp2{{jump if no args for applied func{12939
{{lct{8,wb{8,wa{{else set counter for loop{12940
*      loop to move arguments up on stack
{sapp1{dca{7,xt{{{point to next argument{12944
{{mov{13,num01(xt){9,(xt){{move argument up{12945
{{bct{8,wb{6,sapp1{{loop till all moved{12946
*      merge here to call function (wa = number of arguments)
{sapp2{ica{7,xs{{{adjust stack ptr for apply 1st arg{12950
{{jsr{6,gtnvr{{{get variable block addr for func{12951
{{ppm{6,sapp3{{{jump if not natural variable{12952
{{mov{7,xl{13,vrfnc(xr){{else point to function block{12953
{{brn{6,cfunc{{{go call applied function{12954
*      here for invalid first argument
{sapp3{erb{1,060{26,apply first arg is not natural variable name{{{12958
{{ejc{{{{{12959
*      arbno
*      arbno builds a compound pattern. see description at
*      start of pattern matching section for structure formed.
{s_abn{ent{{{{entry point{12966
{{zer{7,xr{{{set parm1 = 0 for the moment{12967
{{mov{8,wb{22,=p_alt{{set pcode for alternative node{12968
{{jsr{6,pbild{{{build alternative node{12969
{{mov{7,xl{7,xr{{save ptr to alternative pattern{12970
{{mov{8,wb{22,=p_abc{{pcode for p_abc{12971
{{zer{7,xr{{{p0blk{12972
{{jsr{6,pbild{{{build p_abc node{12973
{{mov{13,pthen(xr){7,xl{{put alternative node as successor{12974
{{mov{8,wa{7,xl{{remember alternative node pointer{12975
{{mov{7,xl{7,xr{{copy p_abc node ptr{12976
{{mov{7,xr{9,(xs){{load arbno argument{12977
{{mov{9,(xs){8,wa{{stack alternative node pointer{12978
{{jsr{6,gtpat{{{get arbno argument as pattern{12979
{{err{1,061{26,arbno argument is not pattern{{{12980
{{jsr{6,pconc{{{concat arg with p_abc node{12981
{{mov{7,xl{7,xr{{remember ptr to concd patterns{12982
{{mov{8,wb{22,=p_aba{{pcode for p_aba{12983
{{zer{7,xr{{{p0blk{12984
{{jsr{6,pbild{{{build p_aba node{12985
{{mov{13,pthen(xr){7,xl{{concatenate nodes{12986
{{mov{7,xl{9,(xs){{recall ptr to alternative node{12987
{{mov{13,parm1(xl){7,xr{{point alternative back to argument{12988
{{lcw{7,xr{{{get next code word{12989
{{bri{9,(xr){{{execute next code word{12990
{{ejc{{{{{12991
*      arg
{s_arg{ent{{{{entry point{12995
{{jsr{6,gtsmi{{{get second arg as small integer{12996
{{err{1,062{26,arg second argument is not integer{{{12997
{{ppm{6,exfal{{{fail if out of range or negative{12998
{{mov{8,wa{7,xr{{save argument number{12999
{{mov{7,xr{10,(xs)+{{load first argument{13000
{{jsr{6,gtnvr{{{locate vrblk{13001
{{ppm{6,sarg1{{{jump if not natural variable{13002
{{mov{7,xr{13,vrfnc(xr){{else load function block pointer{13003
{{bne{9,(xr){22,=b_pfc{6,sarg1{jump if not program defined{13004
{{bze{8,wa{6,exfal{{fail if arg number is zero{13005
{{bgt{8,wa{13,fargs(xr){6,exfal{fail if arg number is too large{13006
{{wtb{8,wa{{{else convert to byte offset{13007
{{add{7,xr{8,wa{{point to argument selected{13008
{{mov{7,xr{13,pfagb(xr){{load argument vrblk pointer{13009
{{brn{6,exvnm{{{exit to build nmblk{13010
*      here if 1st argument is bad
{sarg1{erb{1,063{26,arg first argument is not program function name{{{13014
{{ejc{{{{{13015
*      array
{s_arr{ent{{{{entry point{13019
{{mov{7,xl{10,(xs)+{{load initial element value{13020
{{mov{7,xr{10,(xs)+{{load first argument{13021
{{jsr{6,gtint{{{convert first arg to integer{13022
{{ppm{6,sar02{{{jump if not integer{13023
*      here for integer first argument, build vcblk
{{ldi{13,icval(xr){{{load integer value{13027
{{ile{6,sar10{{{jump if zero or neg (bad dimension){13028
{{mfi{8,wa{6,sar11{{else convert to one word, test ovfl{13029
{{jsr{6,vmake{{{create vector{13030
{{ppm{6,sar11{{{fail if too large{13031
{{brn{6,exsid{{{exit setting idval{13032
{{ejc{{{{{13033
*      array (continued)
*      here if first argument is not an integer
{sar02{mov{11,-(xs){7,xr{{replace argument on stack{13039
{{jsr{6,xscni{{{initialize scan of first argument{13040
{{err{1,064{26,array first argument is not integer or string{{{13041
{{ppm{6,exnul{{{dummy (unused) null string exit{13042
{{mov{11,-(xs){3,r_xsc{{save prototype pointer{13043
{{mov{11,-(xs){7,xl{{save default value{13044
{{zer{3,arcdm{{{zero count of dimensions{13045
{{zer{3,arptr{{{zero offset to indicate pass one{13046
{{ldi{4,intv1{{{load integer one{13047
{{sti{3,arnel{{{initialize element count{13048
*      the following code is executed twice. the first time
*      (arptr eq 0), it is used to count the number of elements
*      and number of dimensions. the second time (arptr gt 0) is
*      used to actually fill in the dim,lbd fields of the arblk.
{sar03{ldi{4,intv1{{{load one as default low bound{13055
{{sti{3,arsvl{{{save as low bound{13056
{{mov{8,wc{18,=ch_cl{{set delimiter one = colon{13057
{{mov{7,xl{18,=ch_cm{{set delimiter two = comma{13058
{{zer{8,wa{{{retain blanks in prototype{13059
{{jsr{6,xscan{{{scan next bound{13060
{{bne{8,wa{18,=num01{6,sar04{jump if not colon{13061
*      here we have a colon ending a low bound
{{jsr{6,gtint{{{convert low bound{13065
{{err{1,065{26,array first argument lower bound is not integer{{{13066
{{ldi{13,icval(xr){{{load value of low bound{13067
{{sti{3,arsvl{{{store low bound value{13068
{{mov{8,wc{18,=ch_cm{{set delimiter one = comma{13069
{{mov{7,xl{8,wc{{and delimiter two = comma{13070
{{zer{8,wa{{{retain blanks in prototype{13071
{{jsr{6,xscan{{{scan high bound{13072
{{ejc{{{{{13073
*      array (continued)
*      merge here to process upper bound
{sar04{jsr{6,gtint{{{convert high bound to integer{13079
{{err{1,066{26,array first argument upper bound is not integer{{{13080
{{ldi{13,icval(xr){{{get high bound{13081
{{sbi{3,arsvl{{{subtract lower bound{13082
{{iov{6,sar10{{{bad dimension if overflow{13083
{{ilt{6,sar10{{{bad dimension if negative{13084
{{adi{4,intv1{{{add 1 to get dimension{13085
{{iov{6,sar10{{{bad dimension if overflow{13086
{{mov{7,xl{3,arptr{{load offset (also pass indicator){13087
{{bze{7,xl{6,sar05{{jump if first pass{13088
*      here in second pass to store lbd and dim in arblk
{{add{7,xl{9,(xs){{point to current location in arblk{13092
{{sti{13,cfp_i(xl){{{store dimension{13093
{{ldi{3,arsvl{{{load low bound{13094
{{sti{9,(xl){{{store low bound{13095
{{add{3,arptr{19,*ardms{{bump offset to next bounds{13096
{{brn{6,sar06{{{jump to check for end of bounds{13097
*      here in pass 1
{sar05{icv{3,arcdm{{{bump dimension count{13101
{{mli{3,arnel{{{multiply dimension by count so far{13102
{{iov{6,sar11{{{too large if overflow{13103
{{sti{3,arnel{{{else store updated element count{13104
*      merge here after processing one set of bounds
{sar06{bnz{8,wa{6,sar03{{loop back unless end of bounds{13108
{{bnz{3,arptr{6,sar09{{jump if end of pass 2{13109
{{ejc{{{{{13110
*      array (continued)
*      here at end of pass one, build arblk
{{ldi{3,arnel{{{get number of elements{13116
{{mfi{8,wb{6,sar11{{get as addr integer, test ovflo{13117
{{wtb{8,wb{{{else convert to length in bytes{13118
{{mov{8,wa{19,*arsi_{{set size of standard fields{13119
{{lct{8,wc{3,arcdm{{set dimension count to control loop{13120
*      loop to allow space for dimensions
{sar07{add{8,wa{19,*ardms{{allow space for one set of bounds{13124
{{bct{8,wc{6,sar07{{loop back till all accounted for{13125
{{mov{7,xl{8,wa{{save size (=arofs){13126
*      now allocate space for arblk
{{add{8,wa{8,wb{{add space for elements{13130
{{ica{8,wa{{{allow for arpro prototype field{13131
{{bgt{8,wa{3,mxlen{6,sar11{fail if too large{13132
{{jsr{6,alloc{{{else allocate arblk{13133
{{mov{8,wb{9,(xs){{load default value{13134
{{mov{9,(xs){7,xr{{save arblk pointer{13135
{{mov{8,wc{8,wa{{save length in bytes{13136
{{btw{8,wa{{{convert length back to words{13137
{{lct{8,wa{8,wa{{set counter to control loop{13138
*      loop to clear entire arblk to default value
{sar08{mov{10,(xr)+{8,wb{{set one word{13142
{{bct{8,wa{6,sar08{{loop till all set{13143
{{ejc{{{{{13144
*      array (continued)
*      now set initial fields of arblk
{{mov{7,xr{10,(xs)+{{reload arblk pointer{13150
{{mov{8,wb{9,(xs){{load prototype{13151
{{mov{9,(xr){22,=b_art{{set type word{13152
{{mov{13,arlen(xr){8,wc{{store length in bytes{13153
{{zer{13,idval(xr){{{zero id till we get it built{13154
{{mov{13,arofs(xr){7,xl{{set prototype field ptr{13155
{{mov{13,arndm(xr){3,arcdm{{set number of dimensions{13156
{{mov{8,wc{7,xr{{save arblk pointer{13157
{{add{7,xr{7,xl{{point to prototype field{13158
{{mov{9,(xr){8,wb{{store prototype ptr in arblk{13159
{{mov{3,arptr{19,*arlbd{{set offset for pass 2 bounds scan{13160
{{mov{3,r_xsc{8,wb{{reset string pointer for xscan{13161
{{mov{9,(xs){8,wc{{store arblk pointer on stack{13162
{{zer{3,xsofs{{{reset offset ptr to start of string{13163
{{brn{6,sar03{{{jump back to rescan bounds{13164
*      here after filling in bounds information (end pass two)
{sar09{mov{7,xr{10,(xs)+{{reload pointer to arblk{13168
{{brn{6,exsid{{{exit setting idval{13169
*      here for bad dimension
{sar10{erb{1,067{26,array dimension is zero, negative or out of range{{{13173
*      here if array is too large
{sar11{erb{1,068{26,array size exceeds maximum permitted{{{13177
{{ejc{{{{{13178
*      atan
{s_atn{ent{{{{entry point{13183
{{mov{7,xr{10,(xs)+{{get argument{13184
{{jsr{6,gtrea{{{convert to real{13185
{{err{1,301{26,atan argument not numeric{{{13186
{{ldr{13,rcval(xr){{{load accumulator with argument{13187
{{atn{{{{take arctangent{13188
{{brn{6,exrea{{{overflow, out of range not possible{13189
{{ejc{{{{{13190
{{ejc{{{{{13193
*      backspace
{s_bsp{ent{{{{entry point{13197
{{jsr{6,iofcb{{{call fcblk routine{13198
{{err{1,316{26,backspace argument is not a suitable name{{{13199
{{err{1,316{26,backspace argument is not a suitable name{{{13200
{{err{1,317{26,backspace file does not exist{{{13201
{{jsr{6,sysbs{{{call backspace file function{13202
{{err{1,317{26,backspace file does not exist{{{13203
{{err{1,318{26,backspace file does not permit backspace{{{13204
{{err{1,319{26,backspace caused non-recoverable error{{{13205
{{brn{6,exnul{{{return null as result{13206
{{ejc{{{{{13207
*      break
{s_brk{ent{{{{entry point{13240
{{mov{8,wb{22,=p_bks{{set pcode for single char case{13241
{{mov{7,xl{22,=p_brk{{pcode for multi-char case{13242
{{mov{8,wc{22,=p_bkd{{pcode for expression case{13243
{{jsr{6,patst{{{call common routine to build node{13244
{{err{1,069{26,break argument is not a string or expression{{{13245
{{mov{11,-(xs){7,xr{{stack result{13246
{{lcw{7,xr{{{get next code word{13247
{{bri{9,(xr){{{execute it{13248
{{ejc{{{{{13249
*      breakx
*      breakx is a compound pattern. see description at start
*      of pattern matching section for structure formed.
{s_bkx{ent{{{{entry point{13256
{{mov{8,wb{22,=p_bks{{pcode for single char argument{13257
{{mov{7,xl{22,=p_brk{{pcode for multi-char argument{13258
{{mov{8,wc{22,=p_bxd{{pcode for expression case{13259
{{jsr{6,patst{{{call common routine to build node{13260
{{err{1,070{26,breakx argument is not a string or expression{{{13261
*      now hook breakx node on at front end
{{mov{11,-(xs){7,xr{{save ptr to break node{13265
{{mov{8,wb{22,=p_bkx{{set pcode for breakx node{13266
{{jsr{6,pbild{{{build it{13267
{{mov{13,pthen(xr){9,(xs){{set break node as successor{13268
{{mov{8,wb{22,=p_alt{{set pcode for alternation node{13269
{{jsr{6,pbild{{{build (parm1=alt=breakx node){13270
{{mov{8,wa{7,xr{{save ptr to alternation node{13271
{{mov{7,xr{9,(xs){{point to break node{13272
{{mov{13,pthen(xr){8,wa{{set alternate node as successor{13273
{{lcw{7,xr{{{result on stack{13274
{{bri{9,(xr){{{execute next code word{13275
{{ejc{{{{{13276
*      char
{s_chr{ent{{{{entry point{13280
{{jsr{6,gtsmi{{{convert arg to integer{13281
{{err{1,281{26,char argument not integer{{{13282
{{ppm{6,schr1{{{too big error exit{13283
{{bge{8,wc{18,=cfp_a{6,schr1{see if out of range of host set{13284
{{mov{8,wa{18,=num01{{if not set scblk allocation{13285
{{mov{8,wb{8,wc{{save char code{13286
{{jsr{6,alocs{{{allocate 1 bau scblk{13287
{{mov{7,xl{7,xr{{copy scblk pointer{13288
{{psc{7,xl{{{get set to stuff char{13289
{{sch{8,wb{9,(xl){{stuff it{13290
{{csc{7,xl{{{complete store character{13291
{{zer{7,xl{{{clear slop in xl{13292
{{mov{11,-(xs){7,xr{{stack result{13293
{{lcw{7,xr{{{get next code word{13294
{{bri{9,(xr){{{execute it{13295
*      here if char argument is out of range
{schr1{erb{1,282{26,char argument not in range{{{13299
{{ejc{{{{{13300
*      chop
{s_chp{ent{{{{entry point{13305
{{mov{7,xr{10,(xs)+{{get argument{13306
{{jsr{6,gtrea{{{convert to real{13307
{{err{1,302{26,chop argument not numeric{{{13308
{{ldr{13,rcval(xr){{{load accumulator with argument{13309
{{chp{{{{truncate to integer valued real{13310
{{brn{6,exrea{{{no overflow possible{13311
{{ejc{{{{{13312
*      clear
{s_clr{ent{{{{entry point{13317
{{jsr{6,xscni{{{initialize to scan argument{13318
{{err{1,071{26,clear argument is not a string{{{13319
{{ppm{6,sclr2{{{jump if null{13320
*      loop to scan out names in first argument. variables in
*      the list are flagged by setting vrget of vrblk to zero.
{sclr1{mov{8,wc{18,=ch_cm{{set delimiter one = comma{13325
{{mov{7,xl{8,wc{{delimiter two = comma{13326
{{mnz{8,wa{{{skip/trim blanks in prototype{13327
{{jsr{6,xscan{{{scan next variable name{13328
{{jsr{6,gtnvr{{{locate vrblk{13329
{{err{1,072{26,clear argument has null variable name{{{13330
{{zer{13,vrget(xr){{{else flag by zeroing vrget field{13331
{{bnz{8,wa{6,sclr1{{loop back if stopped by comma{13332
*      here after flagging variables in argument list
{sclr2{mov{8,wb{3,hshtb{{point to start of hash table{13336
*      loop through slots in hash table
{sclr3{beq{8,wb{3,hshte{6,exnul{exit returning null if none left{13340
{{mov{7,xr{8,wb{{else copy slot pointer{13341
{{ica{8,wb{{{bump slot pointer{13342
{{sub{7,xr{19,*vrnxt{{set offset to merge into loop{13343
*      loop through vrblks on one hash chain
{sclr4{mov{7,xr{13,vrnxt(xr){{point to next vrblk on chain{13347
{{bze{7,xr{6,sclr3{{jump for next bucket if chain end{13348
{{bnz{13,vrget(xr){6,sclr5{{jump if not flagged{13349
{{ejc{{{{{13350
*      clear (continued)
*      here for flagged variable, do not set value to null
{{jsr{6,setvr{{{for flagged var, restore vrget{13356
{{brn{6,sclr4{{{and loop back for next vrblk{13357
*      here to set value of a variable to null
*      protected variables (arb, etc) are exempt
{sclr5{beq{13,vrsto(xr){22,=b_vre{6,sclr4{check for protected variable{13362
{{mov{7,xl{7,xr{{copy vrblk pointer{13363
*      loop to locate value at end of possible trblk chain
{sclr6{mov{8,wa{7,xl{{save block pointer{13367
{{mov{7,xl{13,vrval(xl){{load next value field{13368
{{beq{9,(xl){22,=b_trt{6,sclr6{loop back if trapped{13369
*      now store the null value
{{mov{7,xl{8,wa{{restore block pointer{13373
{{mov{13,vrval(xl){21,=nulls{{store null constant value{13374
{{brn{6,sclr4{{{loop back for next vrblk{13375
{{ejc{{{{{13376
*      code
{s_cod{ent{{{{entry point{13380
{{mov{7,xr{10,(xs)+{{load argument{13381
{{jsr{6,gtcod{{{convert to code{13382
{{ppm{6,exfal{{{fail if conversion is impossible{13383
{{mov{11,-(xs){7,xr{{stack result{13384
{{zer{3,r_ccb{{{forget interim code block{13385
{{lcw{7,xr{{{get next code word{13386
{{bri{9,(xr){{{execute it{13387
{{ejc{{{{{13388
*      collect
{s_col{ent{{{{entry point{13392
{{mov{7,xr{10,(xs)+{{load argument{13393
{{jsr{6,gtint{{{convert to integer{13394
{{err{1,073{26,collect argument is not integer{{{13395
{{ldi{13,icval(xr){{{load collect argument{13396
{{sti{3,clsvi{{{save collect argument{13397
{{zer{8,wb{{{set no move up{13398
{{zer{3,r_ccb{{{forget interim code block{13399
{{zer{3,dnams{{{collect sediment too{13401
{{jsr{6,gbcol{{{perform garbage collection{13402
{{mov{3,dnams{7,xr{{record new sediment size{13403
{{mov{8,wa{3,dname{{point to end of memory{13407
{{sub{8,wa{3,dnamp{{subtract next location{13408
{{btw{8,wa{{{convert bytes to words{13409
{{mti{8,wa{{{convert words available as integer{13410
{{sbi{3,clsvi{{{subtract argument{13411
{{iov{6,exfal{{{fail if overflow{13412
{{ilt{6,exfal{{{fail if not enough{13413
{{adi{3,clsvi{{{else recompute available{13414
{{brn{6,exint{{{and exit with integer result{13415
{{ejc{{{{{13416
*      convert
{s_cnv{ent{{{{entry point{13445
{{jsr{6,gtstg{{{convert second argument to string{13446
{{ppm{6,scv29{{{error if second argument not string{13447
{{bze{8,wa{6,scv29{{or if null string{13448
{{mov{7,xl{9,(xs){{load first argument{13452
{{bne{9,(xl){22,=b_pdt{6,scv01{jump if not program defined{13453
*      here for program defined datatype
{{mov{7,xl{13,pddfp(xl){{point to dfblk{13457
{{mov{7,xl{13,dfnam(xl){{load datatype name{13458
{{jsr{6,ident{{{compare with second arg{13459
{{ppm{6,exits{{{exit if ident with arg as result{13460
{{brn{6,exfal{{{else fail{13461
*      here if not program defined datatype
{scv01{mov{11,-(xs){7,xr{{save string argument{13465
{{mov{7,xl{21,=svctb{{point to table of names to compare{13466
{{zer{8,wb{{{initialize counter{13467
{{mov{8,wc{8,wa{{save length of argument string{13468
*      loop through table entries
{scv02{mov{7,xr{10,(xl)+{{load next table entry, bump pointer{13472
{{bze{7,xr{6,exfal{{fail if zero marking end of list{13473
{{bne{8,wc{13,sclen(xr){6,scv05{jump if wrong length{13474
{{mov{3,cnvtp{7,xl{{else store table pointer{13475
{{plc{7,xr{{{point to chars of table entry{13476
{{mov{7,xl{9,(xs){{load pointer to string argument{13477
{{plc{7,xl{{{point to chars of string arg{13478
{{mov{8,wa{8,wc{{set number of chars to compare{13479
{{cmc{6,scv04{6,scv04{{compare, jump if no match{13480
{{ejc{{{{{13481
*      convert (continued)
*      here we have a match
{scv03{mov{7,xl{8,wb{{copy entry number{13487
{{ica{7,xs{{{pop string arg off stack{13488
{{mov{7,xr{10,(xs)+{{load first argument{13489
{{bsw{7,xl{2,cnvtt{{jump to appropriate routine{13490
{{iff{1,0{6,scv06{{string{13508
{{iff{1,1{6,scv07{{integer{13508
{{iff{1,2{6,scv09{{name{13508
{{iff{1,3{6,scv10{{pattern{13508
{{iff{1,4{6,scv11{{array{13508
{{iff{1,5{6,scv19{{table{13508
{{iff{1,6{6,scv25{{expression{13508
{{iff{1,7{6,scv26{{code{13508
{{iff{1,8{6,scv27{{numeric{13508
{{iff{2,cnvrt{6,scv08{{real{13508
{{esw{{{{end of switch table{13508
*      here if no match with table entry
{scv04{mov{7,xl{3,cnvtp{{restore table pointer, merge{13512
*      merge here if lengths did not match
{scv05{icv{8,wb{{{bump entry number{13516
{{brn{6,scv02{{{loop back to check next entry{13517
*      here to convert to string
{scv06{mov{11,-(xs){7,xr{{replace string argument on stack{13521
{{jsr{6,gtstg{{{convert to string{13522
{{ppm{6,exfal{{{fail if conversion not possible{13523
{{mov{11,-(xs){7,xr{{stack result{13524
{{lcw{7,xr{{{get next code word{13525
{{bri{9,(xr){{{execute it{13526
{{ejc{{{{{13527
*      convert (continued)
*      here to convert to integer
{scv07{jsr{6,gtint{{{convert to integer{13533
{{ppm{6,exfal{{{fail if conversion not possible{13534
{{mov{11,-(xs){7,xr{{stack result{13535
{{lcw{7,xr{{{get next code word{13536
{{bri{9,(xr){{{execute it{13537
*      here to convert to real
{scv08{jsr{6,gtrea{{{convert to real{13543
{{ppm{6,exfal{{{fail if conversion not possible{13544
{{mov{11,-(xs){7,xr{{stack result{13545
{{lcw{7,xr{{{get next code word{13546
{{bri{9,(xr){{{execute it{13547
*      here to convert to name
{scv09{beq{9,(xr){22,=b_nml{6,exixr{return if already a name{13552
{{jsr{6,gtnvr{{{else try string to name convert{13553
{{ppm{6,exfal{{{fail if conversion not possible{13554
{{brn{6,exvnm{{{else exit building nmblk for vrblk{13555
*      here to convert to pattern
{scv10{jsr{6,gtpat{{{convert to pattern{13559
{{ppm{6,exfal{{{fail if conversion not possible{13560
{{mov{11,-(xs){7,xr{{stack result{13561
{{lcw{7,xr{{{get next code word{13562
{{bri{9,(xr){{{execute it{13563
*      convert to array
*      if the first argument is a table, then we go through
*      an intermediate array of addresses that is sorted to
*      provide a result ordered by time of entry in the
*      original table.  see c3.762.
{scv11{mov{11,-(xs){7,xr{{save argument on stack{13572
{{zer{8,wa{{{use table chain block addresses{13573
{{jsr{6,gtarr{{{get an array{13574
{{ppm{6,exfal{{{fail if empty table{13575
{{ppm{6,exfal{{{fail if not convertible{13576
{{mov{7,xl{10,(xs)+{{reload original arg{13577
{{bne{9,(xl){22,=b_tbt{6,exsid{exit if original not a table{13578
{{mov{11,-(xs){7,xr{{sort the intermediate array{13579
{{mov{11,-(xs){21,=nulls{{on first column{13580
{{zer{8,wa{{{sort ascending{13581
{{jsr{6,sorta{{{do sort{13582
{{ppm{6,exfal{{{if sort fails, so shall we{13583
{{mov{8,wb{7,xr{{save array result{13584
{{ldi{13,ardim(xr){{{load dim 1 (number of elements){13585
{{mfi{8,wa{{{get as one word integer{13586
{{lct{8,wa{8,wa{{copy to control loop{13587
{{add{7,xr{19,*arvl2{{point to first element in array{13588
*      here for each row of this 2-column array
{scv12{mov{7,xl{9,(xr){{get teblk address{13592
{{mov{10,(xr)+{13,tesub(xl){{replace with subscript{13593
{{mov{10,(xr)+{13,teval(xl){{replace with value{13594
{{bct{8,wa{6,scv12{{loop till all copied over{13595
{{mov{7,xr{8,wb{{retrieve array address{13596
{{brn{6,exsid{{{exit setting id field{13597
*      convert to table
{scv19{mov{8,wa{9,(xr){{load first word of block{13601
{{mov{11,-(xs){7,xr{{replace arblk pointer on stack{13602
{{beq{8,wa{22,=b_tbt{6,exits{return arg if already a table{13603
{{bne{8,wa{22,=b_art{6,exfal{else fail if not an array{13604
{{ejc{{{{{13605
*      convert (continued)
*      here to convert an array to table
{{bne{13,arndm(xr){18,=num02{6,exfal{fail if not 2-dim array{13611
{{ldi{13,ardm2(xr){{{load dim 2{13612
{{sbi{4,intv2{{{subtract 2 to compare{13613
{{ine{6,exfal{{{fail if dim2 not 2{13614
*      here we have an arblk of the right shape
{{ldi{13,ardim(xr){{{load dim 1 (number of elements){13618
{{mfi{8,wa{{{get as one word integer{13619
{{lct{8,wb{8,wa{{copy to control loop{13620
{{add{8,wa{18,=tbsi_{{add space for standard fields{13621
{{wtb{8,wa{{{convert length to bytes{13622
{{jsr{6,alloc{{{allocate space for tbblk{13623
{{mov{8,wc{7,xr{{copy tbblk pointer{13624
{{mov{11,-(xs){7,xr{{save tbblk pointer{13625
{{mov{10,(xr)+{22,=b_tbt{{store type word{13626
{{zer{10,(xr)+{{{store zero for idval for now{13627
{{mov{10,(xr)+{8,wa{{store length{13628
{{mov{10,(xr)+{21,=nulls{{null initial lookup value{13629
*      loop to initialize bucket ptrs to point to table
{scv20{mov{10,(xr)+{8,wc{{set bucket ptr to point to tbblk{13633
{{bct{8,wb{6,scv20{{loop till all initialized{13634
{{mov{8,wb{19,*arvl2{{set offset to first arblk element{13635
*      loop to copy elements from array to table
{scv21{mov{7,xl{13,num01(xs){{point to arblk{13639
{{beq{8,wb{13,arlen(xl){6,scv24{jump if all moved{13640
{{add{7,xl{8,wb{{else point to current location{13641
{{add{8,wb{19,*num02{{bump offset{13642
{{mov{7,xr{9,(xl){{load subscript name{13643
{{dca{7,xl{{{adjust ptr to merge (trval=1+1){13644
{{ejc{{{{{13645
*      convert (continued)
*      loop to chase down trblk chain for value
{scv22{mov{7,xl{13,trval(xl){{point to next value{13651
{{beq{9,(xl){22,=b_trt{6,scv22{loop back if trapped{13652
*      here with name in xr, value in xl
{scv23{mov{11,-(xs){7,xl{{stack value{13656
{{mov{7,xl{13,num01(xs){{load tbblk pointer{13657
{{jsr{6,tfind{{{build teblk (note wb gt 0 by name){13658
{{ppm{6,exfal{{{fail if acess fails{13659
{{mov{13,teval(xl){10,(xs)+{{store value in teblk{13660
{{brn{6,scv21{{{loop back for next element{13661
*      here after moving all elements to tbblk
{scv24{mov{7,xr{10,(xs)+{{load tbblk pointer{13665
{{ica{7,xs{{{pop arblk pointer{13666
{{brn{6,exsid{{{exit setting idval{13667
*      convert to expression
{scv25{zer{8,wb{{{by value{13672
{{jsr{6,gtexp{{{convert to expression{13673
{{ppm{6,exfal{{{fail if conversion not possible{13677
{{zer{3,r_ccb{{{forget interim code block{13678
{{mov{11,-(xs){7,xr{{stack result{13679
{{lcw{7,xr{{{get next code word{13680
{{bri{9,(xr){{{execute it{13681
*      convert to code
{scv26{jsr{6,gtcod{{{convert to code{13685
{{ppm{6,exfal{{{fail if conversion is not possible{13686
{{zer{3,r_ccb{{{forget interim code block{13687
{{mov{11,-(xs){7,xr{{stack result{13688
{{lcw{7,xr{{{get next code word{13689
{{bri{9,(xr){{{execute it{13690
*      convert to numeric
{scv27{jsr{6,gtnum{{{convert to numeric{13694
{{ppm{6,exfal{{{fail if unconvertible{13695
{scv31{mov{11,-(xs){7,xr{{stack result{13696
{{lcw{7,xr{{{get next code word{13697
{{bri{9,(xr){{{execute it{13698
{{ejc{{{{{13699
*      second argument not string or null
{scv29{erb{1,074{26,convert second argument is not a string{{{13725
*      copy
{s_cop{ent{{{{entry point{13729
{{jsr{6,copyb{{{copy the block{13730
{{ppm{6,exits{{{return if no idval field{13731
{{brn{6,exsid{{{exit setting id value{13732
{{ejc{{{{{13733
*      cos
{s_cos{ent{{{{entry point{13738
{{mov{7,xr{10,(xs)+{{get argument{13739
{{jsr{6,gtrea{{{convert to real{13740
{{err{1,303{26,cos argument not numeric{{{13741
{{ldr{13,rcval(xr){{{load accumulator with argument{13742
{{cos{{{{take cosine{13743
{{rno{6,exrea{{{if no overflow, return result in ra{13744
{{erb{1,322{26,cos argument is out of range{{{13745
{{ejc{{{{{13746
*      data
{s_dat{ent{{{{entry point{13751
{{jsr{6,xscni{{{prepare to scan argument{13752
{{err{1,075{26,data argument is not a string{{{13753
{{err{1,076{26,data argument is null{{{13754
*      scan out datatype name
{{mov{8,wc{18,=ch_pp{{delimiter one = left paren{13758
{{mov{7,xl{8,wc{{delimiter two = left paren{13759
{{mnz{8,wa{{{skip/trim blanks in prototype{13760
{{jsr{6,xscan{{{scan datatype name{13761
{{bnz{8,wa{6,sdat1{{skip if left paren found{13762
{{erb{1,077{26,data argument is missing a left paren{{{13763
*      here after scanning datatype name
{sdat1{mov{7,xl{7,xr{{save name ptr{13773
{{mov{8,wa{13,sclen(xr){{get length{13775
{{ctb{8,wa{2,scsi_{{compute space needed{13776
{{jsr{6,alost{{{request static store for name{13777
{{mov{11,-(xs){7,xr{{save datatype name{13778
{{mvw{{{{copy name to static{13779
{{mov{7,xr{9,(xs){{get name ptr{13780
{{zer{7,xl{{{scrub dud register{13781
{{jsr{6,gtnvr{{{locate vrblk for datatype name{13782
{{err{1,078{26,data argument has null datatype name{{{13783
{{mov{3,datdv{7,xr{{save vrblk pointer for datatype{13784
{{mov{3,datxs{7,xs{{store starting stack value{13785
{{zer{8,wb{{{zero count of field names{13786
*      loop to scan field names and stack vrblk pointers
{sdat2{mov{8,wc{18,=ch_rp{{delimiter one = right paren{13790
{{mov{7,xl{18,=ch_cm{{delimiter two = comma{13791
{{mnz{8,wa{{{skip/trim blanks in prototype{13792
{{jsr{6,xscan{{{scan next field name{13793
{{bnz{8,wa{6,sdat3{{jump if delimiter found{13794
{{erb{1,079{26,data argument is missing a right paren{{{13795
*      here after scanning out one field name
{sdat3{jsr{6,gtnvr{{{locate vrblk for field name{13799
{{err{1,080{26,data argument has null field name{{{13800
{{mov{11,-(xs){7,xr{{stack vrblk pointer{13801
{{icv{8,wb{{{increment counter{13802
{{beq{8,wa{18,=num02{6,sdat2{loop back if stopped by comma{13803
{{ejc{{{{{13804
*      data (continued)
*      now build the dfblk
{{mov{8,wa{18,=dfsi_{{set size of dfblk standard fields{13810
{{add{8,wa{8,wb{{add number of fields{13811
{{wtb{8,wa{{{convert length to bytes{13812
{{mov{8,wc{8,wb{{preserve no. of fields{13813
{{jsr{6,alost{{{allocate space for dfblk{13814
{{mov{8,wb{8,wc{{get no of fields{13815
{{mov{7,xt{3,datxs{{point to start of stack{13816
{{mov{8,wc{9,(xt){{load datatype name{13817
{{mov{9,(xt){7,xr{{save dfblk pointer on stack{13818
{{mov{10,(xr)+{22,=b_dfc{{store type word{13819
{{mov{10,(xr)+{8,wb{{store number of fields (fargs){13820
{{mov{10,(xr)+{8,wa{{store length (dflen){13821
{{sub{8,wa{19,*pddfs{{compute pdblk length (for dfpdl){13822
{{mov{10,(xr)+{8,wa{{store pdblk length (dfpdl){13823
{{mov{10,(xr)+{8,wc{{store datatype name (dfnam){13824
{{lct{8,wc{8,wb{{copy number of fields{13825
*      loop to move field name vrblk pointers to dfblk
{sdat4{mov{10,(xr)+{11,-(xt){{move one field name vrblk pointer{13829
{{bct{8,wc{6,sdat4{{loop till all moved{13830
*      now define the datatype function
{{mov{8,wc{8,wa{{copy length of pdblk for later loop{13834
{{mov{7,xr{3,datdv{{point to vrblk{13835
{{mov{7,xt{3,datxs{{point back on stack{13836
{{mov{7,xl{9,(xt){{load dfblk pointer{13837
{{jsr{6,dffnc{{{define function{13838
{{ejc{{{{{13839
*      data (continued)
*      loop to build ffblks
*      notice that the ffblks are constructed in reverse order
*      so that the required offsets can be obtained from
*      successive decrementation of the pdblk length (in wc).
{sdat5{mov{8,wa{19,*ffsi_{{set length of ffblk{13850
{{jsr{6,alloc{{{allocate space for ffblk{13851
{{mov{9,(xr){22,=b_ffc{{set type word{13852
{{mov{13,fargs(xr){18,=num01{{store fargs (always one){13853
{{mov{7,xt{3,datxs{{point back on stack{13854
{{mov{13,ffdfp(xr){9,(xt){{copy dfblk ptr to ffblk{13855
{{dca{8,wc{{{decrement old dfpdl to get next ofs{13856
{{mov{13,ffofs(xr){8,wc{{set offset to this field{13857
{{zer{13,ffnxt(xr){{{tentatively set zero forward ptr{13858
{{mov{7,xl{7,xr{{copy ffblk pointer for dffnc{13859
{{mov{7,xr{9,(xs){{load vrblk pointer for field{13860
{{mov{7,xr{13,vrfnc(xr){{load current function pointer{13861
{{bne{9,(xr){22,=b_ffc{6,sdat6{skip if not currently a field func{13862
*      here we must chain an old ffblk ptr to preserve it in the
*      case of multiple field functions with the same name
{{mov{13,ffnxt(xl){7,xr{{link new ffblk to previous chain{13867
*      merge here to define field function
{sdat6{mov{7,xr{10,(xs)+{{load vrblk pointer{13871
{{jsr{6,dffnc{{{define field function{13872
{{bne{7,xs{3,datxs{6,sdat5{loop back till all done{13873
{{ica{7,xs{{{pop dfblk pointer{13874
{{brn{6,exnul{{{return with null result{13875
{{ejc{{{{{13876
*      datatype
{s_dtp{ent{{{{entry point{13880
{{mov{7,xr{10,(xs)+{{load argument{13881
{{jsr{6,dtype{{{get datatype{13882
{{mov{11,-(xs){7,xr{{stack result{13883
{{lcw{7,xr{{{get next code word{13884
{{bri{9,(xr){{{execute it{13885
{{ejc{{{{{13886
*      date
{s_dte{ent{{{{entry point{13890
{{mov{7,xr{10,(xs)+{{load argument{13891
{{jsr{6,gtint{{{convert to an integer{13892
{{err{1,330{26,date argument is not integer{{{13893
{{jsr{6,sysdt{{{call system date routine{13894
{{mov{8,wa{13,num01(xl){{load length for sbstr{13895
{{bze{8,wa{6,exnul{{return null if length is zero{13896
{{zer{8,wb{{{set zero offset{13897
{{jsr{6,sbstr{{{use sbstr to build scblk{13898
{{mov{11,-(xs){7,xr{{stack result{13899
{{lcw{7,xr{{{get next code word{13900
{{bri{9,(xr){{{execute it{13901
{{ejc{{{{{13902
*      define
{s_def{ent{{{{entry point{13906
{{mov{7,xr{10,(xs)+{{load second argument{13907
{{zer{3,deflb{{{zero label pointer in case null{13908
{{beq{7,xr{21,=nulls{6,sdf01{jump if null second argument{13909
{{jsr{6,gtnvr{{{else find vrblk for label{13910
{{ppm{6,sdf12{{{jump if not a variable name{13911
{{mov{3,deflb{7,xr{{else set specified entry{13912
*      scan function name
{sdf01{jsr{6,xscni{{{prepare to scan first argument{13916
{{err{1,081{26,define first argument is not a string{{{13917
{{err{1,082{26,define first argument is null{{{13918
{{mov{8,wc{18,=ch_pp{{delimiter one = left paren{13919
{{mov{7,xl{8,wc{{delimiter two = left paren{13920
{{mnz{8,wa{{{skip/trim blanks in prototype{13921
{{jsr{6,xscan{{{scan out function name{13922
{{bnz{8,wa{6,sdf02{{jump if left paren found{13923
{{erb{1,083{26,define first argument is missing a left paren{{{13924
*      here after scanning out function name
{sdf02{jsr{6,gtnvr{{{get variable name{13928
{{err{1,084{26,define first argument has null function name{{{13929
{{mov{3,defvr{7,xr{{save vrblk pointer for function nam{13930
{{zer{8,wb{{{zero count of arguments{13931
{{mov{3,defxs{7,xs{{save initial stack pointer{13932
{{bnz{3,deflb{6,sdf03{{jump if second argument given{13933
{{mov{3,deflb{7,xr{{else default is function name{13934
*      loop to scan argument names and stack vrblk pointers
{sdf03{mov{8,wc{18,=ch_rp{{delimiter one = right paren{13938
{{mov{7,xl{18,=ch_cm{{delimiter two = comma{13939
{{mnz{8,wa{{{skip/trim blanks in prototype{13940
{{jsr{6,xscan{{{scan out next argument name{13941
{{bnz{8,wa{6,sdf04{{skip if delimiter found{13942
{{erb{1,085{26,null arg name or missing ) in define first arg.{{{13943
{{ejc{{{{{13944
*      define (continued)
*      here after scanning an argument name
{sdf04{bne{7,xr{21,=nulls{6,sdf05{skip if non-null{13950
{{bze{8,wb{6,sdf06{{ignore null if case of no arguments{13951
*      here after dealing with the case of no arguments
{sdf05{jsr{6,gtnvr{{{get vrblk pointer{13955
{{ppm{6,sdf03{{{loop back to ignore null name{13956
{{mov{11,-(xs){7,xr{{stack argument vrblk pointer{13957
{{icv{8,wb{{{increment counter{13958
{{beq{8,wa{18,=num02{6,sdf03{loop back if stopped by a comma{13959
*      here after scanning out function argument names
{sdf06{mov{3,defna{8,wb{{save number of arguments{13963
{{zer{8,wb{{{zero count of locals{13964
*      loop to scan local names and stack vrblk pointers
{sdf07{mov{8,wc{18,=ch_cm{{set delimiter one = comma{13968
{{mov{7,xl{8,wc{{set delimiter two = comma{13969
{{mnz{8,wa{{{skip/trim blanks in prototype{13970
{{jsr{6,xscan{{{scan out next local name{13971
{{bne{7,xr{21,=nulls{6,sdf08{skip if non-null{13972
{{bze{8,wa{6,sdf09{{exit scan if end of string{13973
*      here after scanning out a local name
{sdf08{jsr{6,gtnvr{{{get vrblk pointer{13977
{{ppm{6,sdf07{{{loop back to ignore null name{13978
{{icv{8,wb{{{if ok, increment count{13979
{{mov{11,-(xs){7,xr{{stack vrblk pointer{13980
{{bnz{8,wa{6,sdf07{{loop back if stopped by a comma{13981
{{ejc{{{{{13982
*      define (continued)
*      here after scanning locals, build pfblk
{sdf09{mov{8,wa{8,wb{{copy count of locals{13988
{{add{8,wa{3,defna{{add number of arguments{13989
{{mov{8,wc{8,wa{{set sum args+locals as loop count{13990
{{add{8,wa{18,=pfsi_{{add space for standard fields{13991
{{wtb{8,wa{{{convert length to bytes{13992
{{jsr{6,alloc{{{allocate space for pfblk{13993
{{mov{7,xl{7,xr{{save pointer to pfblk{13994
{{mov{10,(xr)+{22,=b_pfc{{store first word{13995
{{mov{10,(xr)+{3,defna{{store number of arguments{13996
{{mov{10,(xr)+{8,wa{{store length (pflen){13997
{{mov{10,(xr)+{3,defvr{{store vrblk ptr for function name{13998
{{mov{10,(xr)+{8,wb{{store number of locals{13999
{{zer{10,(xr)+{{{deal with label later{14000
{{zer{10,(xr)+{{{zero pfctr{14001
{{zer{10,(xr)+{{{zero pfrtr{14002
{{bze{8,wc{6,sdf11{{skip if no args or locals{14003
{{mov{8,wa{7,xl{{keep pfblk pointer{14004
{{mov{7,xt{3,defxs{{point before arguments{14005
{{lct{8,wc{8,wc{{get count of args+locals for loop{14006
*      loop to move locals and args to pfblk
{sdf10{mov{10,(xr)+{11,-(xt){{store one entry and bump pointers{14010
{{bct{8,wc{6,sdf10{{loop till all stored{14011
{{mov{7,xl{8,wa{{recover pfblk pointer{14012
{{ejc{{{{{14013
*      define (continued)
*      now deal with label
{sdf11{mov{7,xs{3,defxs{{pop stack{14019
{{mov{13,pfcod(xl){3,deflb{{store label vrblk in pfblk{14020
{{mov{7,xr{3,defvr{{point back to vrblk for function{14021
{{jsr{6,dffnc{{{define function{14022
{{brn{6,exnul{{{and exit returning null{14023
*      here for erroneous label
{sdf12{erb{1,086{26,define function entry point is not defined label{{{14027
{{ejc{{{{{14028
*      detach
{s_det{ent{{{{entry point{14032
{{mov{7,xr{10,(xs)+{{load argument{14033
{{jsr{6,gtvar{{{locate variable{14034
{{err{1,087{26,detach argument is not appropriate name{{{14035
{{jsr{6,dtach{{{detach i/o association from name{14036
{{brn{6,exnul{{{return null result{14037
{{ejc{{{{{14038
*      differ
{s_dif{ent{{{{entry point{14042
{{mov{7,xr{10,(xs)+{{load second argument{14043
{{mov{7,xl{10,(xs)+{{load first argument{14044
{{jsr{6,ident{{{call ident comparison routine{14045
{{ppm{6,exfal{{{fail if ident{14046
{{brn{6,exnul{{{return null if differ{14047
{{ejc{{{{{14048
*      dump
{s_dmp{ent{{{{entry point{14052
{{jsr{6,gtsmi{{{load dump arg as small integer{14053
{{err{1,088{26,dump argument is not integer{{{14054
{{err{1,089{26,dump argument is negative or too large{{{14055
{{jsr{6,dumpr{{{else call dump routine{14056
{{brn{6,exnul{{{and return null as result{14057
{{ejc{{{{{14058
*      dupl
{s_dup{ent{{{{entry point{14062
{{jsr{6,gtsmi{{{get second argument as small integr{14063
{{err{1,090{26,dupl second argument is not integer{{{14064
{{ppm{6,sdup7{{{jump if negative or too big{14065
{{mov{8,wb{7,xr{{save duplication factor{14066
{{jsr{6,gtstg{{{get first arg as string{14067
{{ppm{6,sdup4{{{jump if not a string{14068
*      here for case of duplication of a string
{{mti{8,wa{{{acquire length as integer{14072
{{sti{3,dupsi{{{save for the moment{14073
{{mti{8,wb{{{get duplication factor as integer{14074
{{mli{3,dupsi{{{form product{14075
{{iov{6,sdup3{{{jump if overflow{14076
{{ieq{6,exnul{{{return null if result length = 0{14077
{{mfi{8,wa{6,sdup3{{get as addr integer, check ovflo{14078
*      merge here with result length in wa
{sdup1{mov{7,xl{7,xr{{save string pointer{14082
{{jsr{6,alocs{{{allocate space for string{14083
{{mov{11,-(xs){7,xr{{save as result pointer{14084
{{mov{8,wc{7,xl{{save pointer to argument string{14085
{{psc{7,xr{{{prepare to store chars of result{14086
{{lct{8,wb{8,wb{{set counter to control loop{14087
*      loop through duplications
{sdup2{mov{7,xl{8,wc{{point back to argument string{14091
{{mov{8,wa{13,sclen(xl){{get number of characters{14092
{{plc{7,xl{{{point to chars in argument string{14093
{{mvc{{{{move characters to result string{14094
{{bct{8,wb{6,sdup2{{loop till all duplications done{14095
{{zer{7,xl{{{clear garbage value{14096
{{lcw{7,xr{{{get next code word{14097
{{bri{9,(xr){{{execute next code word{14098
{{ejc{{{{{14099
*      dupl (continued)
*      here if too large, set max length and let alocs catch it
{sdup3{mov{8,wa{3,dname{{set impossible length for alocs{14105
{{brn{6,sdup1{{{merge back{14106
*      here if not a string
{sdup4{jsr{6,gtpat{{{convert argument to pattern{14110
{{err{1,091{26,dupl first argument is not a string or pattern{{{14111
*      here to duplicate a pattern argument
{{mov{11,-(xs){7,xr{{store pattern on stack{14115
{{mov{7,xr{21,=ndnth{{start off with null pattern{14116
{{bze{8,wb{6,sdup6{{null pattern is result if dupfac=0{14117
{{mov{11,-(xs){8,wb{{preserve loop count{14118
*      loop to duplicate by successive concatenation
{sdup5{mov{7,xl{7,xr{{copy current value as right argumnt{14122
{{mov{7,xr{13,num01(xs){{get a new copy of left{14123
{{jsr{6,pconc{{{concatenate{14124
{{dcv{9,(xs){{{count down{14125
{{bnz{9,(xs){6,sdup5{{loop{14126
{{ica{7,xs{{{pop loop count{14127
*      here to exit after constructing pattern
{sdup6{mov{9,(xs){7,xr{{store result on stack{14131
{{lcw{7,xr{{{get next code word{14132
{{bri{9,(xr){{{execute next code word{14133
*      fail if second arg is out of range
{sdup7{ica{7,xs{{{pop first argument{14137
{{brn{6,exfal{{{fail{14138
{{ejc{{{{{14139
*      eject
{s_ejc{ent{{{{entry point{14143
{{jsr{6,iofcb{{{call fcblk routine{14144
{{err{1,092{26,eject argument is not a suitable name{{{14145
{{ppm{6,sejc1{{{null argument{14146
{{err{1,093{26,eject file does not exist{{{14147
{{jsr{6,sysef{{{call eject file function{14148
{{err{1,093{26,eject file does not exist{{{14149
{{err{1,094{26,eject file does not permit page eject{{{14150
{{err{1,095{26,eject caused non-recoverable output error{{{14151
{{brn{6,exnul{{{return null as result{14152
*      here to eject standard output file
{sejc1{jsr{6,sysep{{{call routine to eject printer{14156
{{brn{6,exnul{{{exit with null result{14157
{{ejc{{{{{14158
*      endfile
{s_enf{ent{{{{entry point{14162
{{jsr{6,iofcb{{{call fcblk routine{14163
{{err{1,096{26,endfile argument is not a suitable name{{{14164
{{err{1,097{26,endfile argument is null{{{14165
{{err{1,098{26,endfile file does not exist{{{14166
{{jsr{6,sysen{{{call endfile routine{14167
{{err{1,098{26,endfile file does not exist{{{14168
{{err{1,099{26,endfile file does not permit endfile{{{14169
{{err{1,100{26,endfile caused non-recoverable output error{{{14170
{{mov{8,wb{7,xl{{remember vrblk ptr from iofcb call{14171
{{mov{7,xr{7,xl{{copy pointer{14172
*      loop to find trtrf block
{senf1{mov{7,xl{7,xr{{remember previous entry{14176
{{mov{7,xr{13,trval(xr){{chain along{14177
{{bne{9,(xr){22,=b_trt{6,exnul{skip out if chain end{14178
{{bne{13,trtyp(xr){18,=trtfc{6,senf1{loop if not found{14179
{{mov{13,trval(xl){13,trval(xr){{remove trtrf{14180
{{mov{3,enfch{13,trtrf(xr){{point to head of iochn{14181
{{mov{8,wc{13,trfpt(xr){{point to fcblk{14182
{{mov{7,xr{8,wb{{filearg1 vrblk from iofcb{14183
{{jsr{6,setvr{{{reset it{14184
{{mov{7,xl{20,=r_fcb{{ptr to head of fcblk chain{14185
{{sub{7,xl{19,*num02{{adjust ready to enter loop{14186
*      find fcblk
{senf2{mov{7,xr{7,xl{{copy ptr{14190
{{mov{7,xl{13,num02(xl){{get next link{14191
{{bze{7,xl{6,senf4{{stop if chain end{14192
{{beq{13,num03(xl){8,wc{6,senf3{jump if fcblk found{14193
{{brn{6,senf2{{{loop{14194
*      remove fcblk
{senf3{mov{13,num02(xr){13,num02(xl){{delete fcblk from chain{14198
*      loop which detaches all vbls on iochn chain
{senf4{mov{7,xl{3,enfch{{get chain head{14202
{{bze{7,xl{6,exnul{{finished if chain end{14203
{{mov{3,enfch{13,trtrf(xl){{chain along{14204
{{mov{8,wa{13,ionmo(xl){{name offset{14205
{{mov{7,xl{13,ionmb(xl){{name base{14206
{{jsr{6,dtach{{{detach name{14207
{{brn{6,senf4{{{loop till done{14208
{{ejc{{{{{14209
*      eq
{s_eqf{ent{{{{entry point{14213
{{jsr{6,acomp{{{call arithmetic comparison routine{14214
{{err{1,101{26,eq first argument is not numeric{{{14215
{{err{1,102{26,eq second argument is not numeric{{{14216
{{ppm{6,exfal{{{fail if lt{14217
{{ppm{6,exnul{{{return null if eq{14218
{{ppm{6,exfal{{{fail if gt{14219
{{ejc{{{{{14220
*      eval
{s_evl{ent{{{{entry point{14224
{{mov{7,xr{10,(xs)+{{load argument{14225
{{lcw{8,wc{{{load next code word{14231
{{bne{8,wc{21,=ofne_{6,sevl1{jump if called by value{14232
{{scp{7,xl{{{copy code pointer{14233
{{mov{8,wa{9,(xl){{get next code word{14234
{{bne{8,wa{21,=ornm_{6,sevl2{by name unless expression{14235
{{bnz{13,num01(xs){6,sevl2{{jump if by name{14236
*      here if called by value
{sevl1{zer{8,wb{{{set flag for by value{14240
{{mov{11,-(xs){8,wc{{save code word{14242
{{jsr{6,gtexp{{{convert to expression{14243
{{err{1,103{26,eval argument is not expression{{{14244
{{zer{3,r_ccb{{{forget interim code block{14245
{{zer{8,wb{{{set flag for by value{14246
{{jsr{6,evalx{{{evaluate expression by value{14250
{{ppm{6,exfal{{{fail if evaluation fails{14251
{{mov{7,xl{7,xr{{copy result{14252
{{mov{7,xr{9,(xs){{reload next code word{14253
{{mov{9,(xs){7,xl{{stack result{14254
{{bri{9,(xr){{{jump to execute next code word{14255
*      here if called by name
{sevl2{mov{8,wb{18,=num01{{set flag for by name{14259
{{jsr{6,gtexp{{{convert to expression{14261
{{err{1,103{26,eval argument is not expression{{{14262
{{zer{3,r_ccb{{{forget interim code block{14263
{{mov{8,wb{18,=num01{{set flag for by name{14264
{{jsr{6,evalx{{{evaluate expression by name{14266
{{ppm{6,exfal{{{fail if evaluation fails{14267
{{brn{6,exnam{{{exit with name{14268
{{ejc{{{{{14271
*      exit
{s_ext{ent{{{{entry point{14275
{{zer{8,wb{{{clear amount of static shift{14276
{{zer{3,r_ccb{{{forget interim code block{14277
{{zer{3,dnams{{{collect sediment too{14279
{{jsr{6,gbcol{{{compact memory by collecting{14280
{{mov{3,dnams{7,xr{{record new sediment size{14281
{{jsr{6,gtstg{{{{14285
{{err{1,288{26,exit second argument is not a string{{{14286
{{mov{7,xl{7,xr{{copy second arg string pointer{14287
{{jsr{6,gtstg{{{convert arg to string{14288
{{err{1,104{26,exit first argument is not suitable integer or string{{{14289
{{mov{11,-(xs){7,xl{{save second argument{14290
{{mov{7,xl{7,xr{{copy first arg string ptr{14291
{{jsr{6,gtint{{{check it is integer{14292
{{ppm{6,sext1{{{skip if unconvertible{14293
{{zer{7,xl{{{note it is integer{14294
{{ldi{13,icval(xr){{{get integer arg{14295
*      merge to call osint exit routine
{sext1{mov{8,wb{3,r_fcb{{get fcblk chain header{14299
{{mov{7,xr{21,=headv{{point to v.v string{14300
{{mov{8,wa{10,(xs)+{{provide second argument scblk{14301
{{jsr{6,sysxi{{{call external routine{14302
{{err{1,105{26,exit action not available in this implementation{{{14303
{{err{1,106{26,exit action caused irrecoverable error{{{14304
{{ieq{6,exnul{{{return if argument 0{14305
{{igt{6,sext2{{{skip if positive{14306
{{ngi{{{{make positive{14307
*      check for option respecification
*      sysxi returns 0 in wa when a file has been resumed,
*      1 when this is a continuation of an exit(4) or exit(-4)
*      action.
{sext2{mfi{8,wc{{{get value in work reg{14315
{{add{8,wa{8,wc{{prepare to test for continue{14316
{{beq{8,wa{18,=num05{6,sext5{continued execution if 4 plus 1{14317
{{zer{3,gbcnt{{{resuming execution so reset{14318
{{bge{8,wc{18,=num03{6,sext3{skip if was 3 or 4{14319
{{mov{11,-(xs){8,wc{{save value{14320
{{zer{8,wc{{{set to read options{14321
{{jsr{6,prpar{{{read syspp options{14322
{{mov{8,wc{10,(xs)+{{restore value{14323
*      deal with header option (fiddled by prpar)
{sext3{mnz{3,headp{{{assume no headers{14327
{{bne{8,wc{18,=num01{6,sext4{skip if not 1{14328
{{zer{3,headp{{{request header printing{14329
*      almost ready to resume running
{sext4{jsr{6,systm{{{get execution time start (sgd11){14333
{{sti{3,timsx{{{save as initial time{14334
{{ldi{3,kvstc{{{reset to ensure ...{14335
{{sti{3,kvstl{{{... correct execution stats{14336
{{jsr{6,stgcc{{{recompute countdown counters{14337
{{brn{6,exnul{{{resume execution{14338
*      here after exit(4) or exit(-4) -- create save file
*      or load module and continue execution.
*      return integer 1 to signal the continuation of the
*      original execution.
{sext5{mov{7,xr{21,=inton{{integer one{14346
{{brn{6,exixr{{{return as result{14347
{{ejc{{{{{14349
*      exp
{s_exp{ent{{{{entry point{14354
{{mov{7,xr{10,(xs)+{{get argument{14355
{{jsr{6,gtrea{{{convert to real{14356
{{err{1,304{26,exp argument not numeric{{{14357
{{ldr{13,rcval(xr){{{load accumulator with argument{14358
{{etx{{{{take exponential{14359
{{rno{6,exrea{{{if no overflow, return result in ra{14360
{{erb{1,305{26,exp produced real overflow{{{14361
{{ejc{{{{{14362
*      field
{s_fld{ent{{{{entry point{14367
{{jsr{6,gtsmi{{{get second argument (field number){14368
{{err{1,107{26,field second argument is not integer{{{14369
{{ppm{6,exfal{{{fail if out of range{14370
{{mov{8,wb{7,xr{{else save integer value{14371
{{mov{7,xr{10,(xs)+{{load first argument{14372
{{jsr{6,gtnvr{{{point to vrblk{14373
{{ppm{6,sfld1{{{jump (error) if not variable name{14374
{{mov{7,xr{13,vrfnc(xr){{else point to function block{14375
{{bne{9,(xr){22,=b_dfc{6,sfld1{error if not datatype function{14376
*      here if first argument is a datatype function name
{{bze{8,wb{6,exfal{{fail if argument number is zero{14380
{{bgt{8,wb{13,fargs(xr){6,exfal{fail if too large{14381
{{wtb{8,wb{{{else convert to byte offset{14382
{{add{7,xr{8,wb{{point to field name{14383
{{mov{7,xr{13,dfflb(xr){{load vrblk pointer{14384
{{brn{6,exvnm{{{exit to build nmblk{14385
*      here for bad first argument
{sfld1{erb{1,108{26,field first argument is not datatype name{{{14389
{{ejc{{{{{14390
*      fence
{s_fnc{ent{{{{entry point{14394
{{mov{8,wb{22,=p_fnc{{set pcode for p_fnc{14395
{{zer{7,xr{{{p0blk{14396
{{jsr{6,pbild{{{build p_fnc node{14397
{{mov{7,xl{7,xr{{save pointer to it{14398
{{mov{7,xr{10,(xs)+{{get argument{14399
{{jsr{6,gtpat{{{convert to pattern{14400
{{err{1,259{26,fence argument is not pattern{{{14401
{{jsr{6,pconc{{{concatenate to p_fnc node{14402
{{mov{7,xl{7,xr{{save ptr to concatenated pattern{14403
{{mov{8,wb{22,=p_fna{{set for p_fna pcode{14404
{{zer{7,xr{{{p0blk{14405
{{jsr{6,pbild{{{construct p_fna node{14406
{{mov{13,pthen(xr){7,xl{{set pattern as pthen{14407
{{mov{11,-(xs){7,xr{{set as result{14408
{{lcw{7,xr{{{get next code word{14409
{{bri{9,(xr){{{execute next code word{14410
{{ejc{{{{{14411
*      ge
{s_gef{ent{{{{entry point{14415
{{jsr{6,acomp{{{call arithmetic comparison routine{14416
{{err{1,109{26,ge first argument is not numeric{{{14417
{{err{1,110{26,ge second argument is not numeric{{{14418
{{ppm{6,exfal{{{fail if lt{14419
{{ppm{6,exnul{{{return null if eq{14420
{{ppm{6,exnul{{{return null if gt{14421
{{ejc{{{{{14422
*      gt
{s_gtf{ent{{{{entry point{14426
{{jsr{6,acomp{{{call arithmetic comparison routine{14427
{{err{1,111{26,gt first argument is not numeric{{{14428
{{err{1,112{26,gt second argument is not numeric{{{14429
{{ppm{6,exfal{{{fail if lt{14430
{{ppm{6,exfal{{{fail if eq{14431
{{ppm{6,exnul{{{return null if gt{14432
{{ejc{{{{{14433
*      host
{s_hst{ent{{{{entry point{14437
{{mov{8,wc{10,(xs)+{{get fifth arg{14438
{{mov{8,wb{10,(xs)+{{get fourth arg{14439
{{mov{7,xr{10,(xs)+{{get third arg{14440
{{mov{7,xl{10,(xs)+{{get second arg{14441
{{mov{8,wa{10,(xs)+{{get first arg{14442
{{jsr{6,syshs{{{enter syshs routine{14443
{{err{1,254{26,erroneous argument for host{{{14444
{{err{1,255{26,error during execution of host{{{14445
{{ppm{6,shst1{{{store host string{14446
{{ppm{6,exnul{{{return null result{14447
{{ppm{6,exixr{{{return xr{14448
{{ppm{6,exfal{{{fail return{14449
{{ppm{6,shst3{{{store actual string{14450
{{ppm{6,shst4{{{return copy of xr{14451
*      return host string
{shst1{bze{7,xl{6,exnul{{null string if syshs uncooperative{14455
{{mov{8,wa{13,sclen(xl){{length{14456
{{zer{8,wb{{{zero offset{14457
*      copy string and return
{shst2{jsr{6,sbstr{{{build copy of string{14461
{{mov{11,-(xs){7,xr{{stack the result{14462
{{lcw{7,xr{{{load next code word{14463
{{bri{9,(xr){{{execute it{14464
*      return actual string pointed to by xl
{shst3{zer{8,wb{{{treat xl like an scblk ptr{14468
{{sub{8,wb{18,=cfp_f{{by creating a negative offset{14469
{{brn{6,shst2{{{join to copy string{14470
*      return copy of block pointed to by xr
{shst4{mov{11,-(xs){7,xr{{stack results{14474
{{jsr{6,copyb{{{make copy of block{14475
{{ppm{6,exits{{{if not an aggregate structure{14476
{{brn{6,exsid{{{set current id value otherwise{14477
{{ejc{{{{{14478
*      ident
{s_idn{ent{{{{entry point{14482
{{mov{7,xr{10,(xs)+{{load second argument{14483
{{mov{7,xl{10,(xs)+{{load first argument{14484
{{jsr{6,ident{{{call ident comparison routine{14485
{{ppm{6,exnul{{{return null if ident{14486
{{brn{6,exfal{{{fail if differ{14487
{{ejc{{{{{14488
*      input
{s_inp{ent{{{{entry point{14492
{{zer{8,wb{{{input flag{14493
{{jsr{6,ioput{{{call input/output assoc. routine{14494
{{err{1,113{26,input third argument is not a string{{{14495
{{err{1,114{26,inappropriate second argument for input{{{14496
{{err{1,115{26,inappropriate first argument for input{{{14497
{{err{1,116{26,inappropriate file specification for input{{{14498
{{ppm{6,exfal{{{fail if file does not exist{14499
{{err{1,117{26,input file cannot be read{{{14500
{{err{1,289{26,input channel currently in use{{{14501
{{brn{6,exnul{{{return null string{14502
{{ejc{{{{{14503
*      integer
{s_int{ent{{{{entry point{14536
{{mov{7,xr{10,(xs)+{{load argument{14537
{{jsr{6,gtnum{{{convert to numeric{14538
{{ppm{6,exfal{{{fail if non-numeric{14539
{{beq{8,wa{22,=b_icl{6,exnul{return null if integer{14540
{{brn{6,exfal{{{fail if real{14541
{{ejc{{{{{14542
*      item
*      item does not permit the direct (fast) call so that
*      wa contains the actual number of arguments passed.
{s_itm{ent{{{{entry point{14549
*      deal with case of no args
{{bnz{8,wa{6,sitm1{{jump if at least one arg{14553
{{mov{11,-(xs){21,=nulls{{else supply garbage null arg{14554
{{mov{8,wa{18,=num01{{and fix argument count{14555
*      check for name/value cases
{sitm1{scp{7,xr{{{get current code pointer{14559
{{mov{7,xl{9,(xr){{load next code word{14560
{{dcv{8,wa{{{get number of subscripts{14561
{{mov{7,xr{8,wa{{copy for arref{14562
{{beq{7,xl{21,=ofne_{6,sitm2{jump if called by name{14563
*      here if called by value
{{zer{8,wb{{{set code for call by value{14567
{{brn{6,arref{{{off to array reference routine{14568
*      here for call by name
{sitm2{mnz{8,wb{{{set code for call by name{14572
{{lcw{8,wa{{{load and ignore ofne_ call{14573
{{brn{6,arref{{{off to array reference routine{14574
{{ejc{{{{{14575
*      le
{s_lef{ent{{{{entry point{14579
{{jsr{6,acomp{{{call arithmetic comparison routine{14580
{{err{1,118{26,le first argument is not numeric{{{14581
{{err{1,119{26,le second argument is not numeric{{{14582
{{ppm{6,exnul{{{return null if lt{14583
{{ppm{6,exnul{{{return null if eq{14584
{{ppm{6,exfal{{{fail if gt{14585
{{ejc{{{{{14586
*      len
{s_len{ent{{{{entry point{14590
{{mov{8,wb{22,=p_len{{set pcode for integer arg case{14591
{{mov{8,wa{22,=p_lnd{{set pcode for expr arg case{14592
{{jsr{6,patin{{{call common routine to build node{14593
{{err{1,120{26,len argument is not integer or expression{{{14594
{{err{1,121{26,len argument is negative or too large{{{14595
{{mov{11,-(xs){7,xr{{stack result{14596
{{lcw{7,xr{{{get next code word{14597
{{bri{9,(xr){{{execute it{14598
{{ejc{{{{{14599
*      leq
{s_leq{ent{{{{entry point{14603
{{jsr{6,lcomp{{{call string comparison routine{14604
{{err{1,122{26,leq first argument is not a string{{{14605
{{err{1,123{26,leq second argument is not a string{{{14606
{{ppm{6,exfal{{{fail if llt{14607
{{ppm{6,exnul{{{return null if leq{14608
{{ppm{6,exfal{{{fail if lgt{14609
{{ejc{{{{{14610
*      lge
{s_lge{ent{{{{entry point{14614
{{jsr{6,lcomp{{{call string comparison routine{14615
{{err{1,124{26,lge first argument is not a string{{{14616
{{err{1,125{26,lge second argument is not a string{{{14617
{{ppm{6,exfal{{{fail if llt{14618
{{ppm{6,exnul{{{return null if leq{14619
{{ppm{6,exnul{{{return null if lgt{14620
{{ejc{{{{{14621
*      lgt
{s_lgt{ent{{{{entry point{14625
{{jsr{6,lcomp{{{call string comparison routine{14626
{{err{1,126{26,lgt first argument is not a string{{{14627
{{err{1,127{26,lgt second argument is not a string{{{14628
{{ppm{6,exfal{{{fail if llt{14629
{{ppm{6,exfal{{{fail if leq{14630
{{ppm{6,exnul{{{return null if lgt{14631
{{ejc{{{{{14632
*      lle
{s_lle{ent{{{{entry point{14636
{{jsr{6,lcomp{{{call string comparison routine{14637
{{err{1,128{26,lle first argument is not a string{{{14638
{{err{1,129{26,lle second argument is not a string{{{14639
{{ppm{6,exnul{{{return null if llt{14640
{{ppm{6,exnul{{{return null if leq{14641
{{ppm{6,exfal{{{fail if lgt{14642
{{ejc{{{{{14643
*      llt
{s_llt{ent{{{{entry point{14647
{{jsr{6,lcomp{{{call string comparison routine{14648
{{err{1,130{26,llt first argument is not a string{{{14649
{{err{1,131{26,llt second argument is not a string{{{14650
{{ppm{6,exnul{{{return null if llt{14651
{{ppm{6,exfal{{{fail if leq{14652
{{ppm{6,exfal{{{fail if lgt{14653
{{ejc{{{{{14654
*      lne
{s_lne{ent{{{{entry point{14658
{{jsr{6,lcomp{{{call string comparison routine{14659
{{err{1,132{26,lne first argument is not a string{{{14660
{{err{1,133{26,lne second argument is not a string{{{14661
{{ppm{6,exnul{{{return null if llt{14662
{{ppm{6,exfal{{{fail if leq{14663
{{ppm{6,exnul{{{return null if lgt{14664
{{ejc{{{{{14665
*      ln
{s_lnf{ent{{{{entry point{14670
{{mov{7,xr{10,(xs)+{{get argument{14671
{{jsr{6,gtrea{{{convert to real{14672
{{err{1,306{26,ln argument not numeric{{{14673
{{ldr{13,rcval(xr){{{load accumulator with argument{14674
{{req{6,slnf1{{{overflow if argument is 0{14675
{{rlt{6,slnf2{{{error if argument less than 0{14676
{{lnf{{{{take natural logarithm{14677
{{rno{6,exrea{{{if no overflow, return result in ra{14678
{slnf1{erb{1,307{26,ln produced real overflow{{{14679
*      here for bad argument
{slnf2{erb{1,315{26,ln argument negative{{{14683
{{ejc{{{{{14684
*      local
{s_loc{ent{{{{entry point{14689
{{jsr{6,gtsmi{{{get second argument (local number){14690
{{err{1,134{26,local second argument is not integer{{{14691
{{ppm{6,exfal{{{fail if out of range{14692
{{mov{8,wb{7,xr{{save local number{14693
{{mov{7,xr{10,(xs)+{{load first argument{14694
{{jsr{6,gtnvr{{{point to vrblk{14695
{{ppm{6,sloc1{{{jump if not variable name{14696
{{mov{7,xr{13,vrfnc(xr){{else load function pointer{14697
{{bne{9,(xr){22,=b_pfc{6,sloc1{jump if not program defined{14698
*      here if we have a program defined function name
{{bze{8,wb{6,exfal{{fail if second arg is zero{14702
{{bgt{8,wb{13,pfnlo(xr){6,exfal{or too large{14703
{{add{8,wb{13,fargs(xr){{else adjust offset to include args{14704
{{wtb{8,wb{{{convert to bytes{14705
{{add{7,xr{8,wb{{point to local pointer{14706
{{mov{7,xr{13,pfagb(xr){{load vrblk pointer{14707
{{brn{6,exvnm{{{exit building nmblk{14708
*      here if first argument is no good
{sloc1{erb{1,135{26,local first arg is not a program function name{{{14712
{{ejc{{{{{14715
*      load
{s_lod{ent{{{{entry point{14719
{{jsr{6,gtstg{{{load library name{14720
{{err{1,136{26,load second argument is not a string{{{14721
{{mov{7,xl{7,xr{{save library name{14722
{{jsr{6,xscni{{{prepare to scan first argument{14723
{{err{1,137{26,load first argument is not a string{{{14724
{{err{1,138{26,load first argument is null{{{14725
{{mov{11,-(xs){7,xl{{stack library name{14726
{{mov{8,wc{18,=ch_pp{{set delimiter one = left paren{14727
{{mov{7,xl{8,wc{{set delimiter two = left paren{14728
{{mnz{8,wa{{{skip/trim blanks in prototype{14729
{{jsr{6,xscan{{{scan function name{14730
{{mov{11,-(xs){7,xr{{save ptr to function name{14731
{{bnz{8,wa{6,slod1{{jump if left paren found{14732
{{erb{1,139{26,load first argument is missing a left paren{{{14733
*      here after successfully scanning function name
{slod1{jsr{6,gtnvr{{{locate vrblk{14737
{{err{1,140{26,load first argument has null function name{{{14738
{{mov{3,lodfn{7,xr{{save vrblk pointer{14739
{{zer{3,lodna{{{zero count of arguments{14740
*      loop to scan argument datatype names
{slod2{mov{8,wc{18,=ch_rp{{delimiter one is right paren{14744
{{mov{7,xl{18,=ch_cm{{delimiter two is comma{14745
{{mnz{8,wa{{{skip/trim blanks in prototype{14746
{{jsr{6,xscan{{{scan next argument name{14747
{{icv{3,lodna{{{bump argument count{14748
{{bnz{8,wa{6,slod3{{jump if ok delimiter was found{14749
{{erb{1,141{26,load first argument is missing a right paren{{{14750
{{ejc{{{{{14751
*      load (continued)
*      come here to analyze the datatype pointer in (xr). this
*      code is used both for arguments (wa=1,2) and for the
*      result datatype (with wa set to zero).
{slod3{mov{11,-(xs){7,xr{{stack datatype name pointer{14767
{{mov{8,wb{18,=num01{{set string code in case{14769
{{mov{7,xl{21,=scstr{{point to /string/{14770
{{jsr{6,ident{{{check for match{14771
{{ppm{6,slod4{{{jump if match{14772
{{mov{7,xr{9,(xs){{else reload name{14773
{{add{8,wb{8,wb{{set code for integer (2){14774
{{mov{7,xl{21,=scint{{point to /integer/{14775
{{jsr{6,ident{{{check for match{14776
{{ppm{6,slod4{{{jump if match{14777
{{mov{7,xr{9,(xs){{else reload string pointer{14780
{{icv{8,wb{{{set code for real (3){14781
{{mov{7,xl{21,=screa{{point to /real/{14782
{{jsr{6,ident{{{check for match{14783
{{ppm{6,slod4{{{jump if match{14784
{{mov{7,xr{9,(xs){{reload string pointer{14787
{{icv{8,wb{{{code for file (4, or 3 if no reals){14788
{{mov{7,xl{21,=scfil{{point to /file/{14789
{{jsr{6,ident{{{check for match{14790
{{ppm{6,slod4{{{jump if match{14791
{{zer{8,wb{{{else get code for no convert{14793
*      merge here with proper datatype code in wb
{slod4{mov{9,(xs){8,wb{{store code on stack{14797
{{beq{8,wa{18,=num02{6,slod2{loop back if arg stopped by comma{14798
{{bze{8,wa{6,slod5{{jump if that was the result type{14799
*      here we scan out the result type (arg stopped by ) )
{{mov{8,wc{3,mxlen{{set dummy (impossible) delimiter 1{14803
{{mov{7,xl{8,wc{{and delimiter two{14804
{{mnz{8,wa{{{skip/trim blanks in prototype{14805
{{jsr{6,xscan{{{scan result name{14806
{{zer{8,wa{{{set code for processing result{14807
{{brn{6,slod3{{{jump back to process result name{14808
{{ejc{{{{{14809
*      load (continued)
*      here after processing all args and result
{slod5{mov{8,wa{3,lodna{{get number of arguments{14815
{{mov{8,wc{8,wa{{copy for later{14816
{{wtb{8,wa{{{convert length to bytes{14817
{{add{8,wa{19,*efsi_{{add space for standard fields{14818
{{jsr{6,alloc{{{allocate efblk{14819
{{mov{9,(xr){22,=b_efc{{set type word{14820
{{mov{13,fargs(xr){8,wc{{set number of arguments{14821
{{zer{13,efuse(xr){{{set use count (dffnc will set to 1){14822
{{zer{13,efcod(xr){{{zero code pointer for now{14823
{{mov{13,efrsl(xr){10,(xs)+{{store result type code{14824
{{mov{13,efvar(xr){3,lodfn{{store function vrblk pointer{14825
{{mov{13,eflen(xr){8,wa{{store efblk length{14826
{{mov{8,wb{7,xr{{save efblk pointer{14827
{{add{7,xr{8,wa{{point past end of efblk{14828
{{lct{8,wc{8,wc{{set number of arguments for loop{14829
*      loop to set argument type codes from stack
{slod6{mov{11,-(xr){10,(xs)+{{store one type code from stack{14833
{{bct{8,wc{6,slod6{{loop till all stored{14834
*      now load the external function and perform definition
{{mov{7,xr{10,(xs)+{{load function string name{14838
{{mov{7,xl{9,(xs){{load library name{14843
{{mov{9,(xs){8,wb{{store efblk pointer{14844
{{jsr{6,sysld{{{call function to load external func{14845
{{err{1,142{26,load function does not exist{{{14846
{{err{1,143{26,load function caused input error during load{{{14847
{{err{1,328{26,load function - insufficient memory{{{14848
{{mov{7,xl{10,(xs)+{{recall efblk pointer{14849
{{mov{13,efcod(xl){7,xr{{store code pointer{14850
{{mov{7,xr{3,lodfn{{point to vrblk for function{14851
{{jsr{6,dffnc{{{perform function definition{14852
{{brn{6,exnul{{{return null result{14853
{{ejc{{{{{14855
*      lpad
{s_lpd{ent{{{{entry point{14859
{{jsr{6,gtstg{{{get pad character{14860
{{err{1,144{26,lpad third argument is not a string{{{14861
{{plc{7,xr{{{point to character (null is blank){14862
{{lch{8,wb{9,(xr){{load pad character{14863
{{jsr{6,gtsmi{{{get pad length{14864
{{err{1,145{26,lpad second argument is not integer{{{14865
{{ppm{6,slpd4{{{skip if negative or large{14866
*      merge to check first arg
{slpd1{jsr{6,gtstg{{{get first argument (string to pad){14870
{{err{1,146{26,lpad first argument is not a string{{{14871
{{bge{8,wa{8,wc{6,exixr{return 1st arg if too long to pad{14872
{{mov{7,xl{7,xr{{else move ptr to string to pad{14873
*      now we are ready for the pad
*      (xl)                  pointer to string to pad
*      (wb)                  pad character
*      (wc)                  length to pad string to
{{mov{8,wa{8,wc{{copy length{14881
{{jsr{6,alocs{{{allocate scblk for new string{14882
{{mov{11,-(xs){7,xr{{save as result{14883
{{mov{8,wa{13,sclen(xl){{load length of argument{14884
{{sub{8,wc{8,wa{{calculate number of pad characters{14885
{{psc{7,xr{{{point to chars in result string{14886
{{lct{8,wc{8,wc{{set counter for pad loop{14887
*      loop to perform pad
{slpd2{sch{8,wb{10,(xr)+{{store pad character, bump ptr{14891
{{bct{8,wc{6,slpd2{{loop till all pad chars stored{14892
{{csc{7,xr{{{complete store characters{14893
*      now copy string
{{bze{8,wa{6,slpd3{{exit if null string{14897
{{plc{7,xl{{{else point to chars in argument{14898
{{mvc{{{{move characters to result string{14899
{{zer{7,xl{{{clear garbage xl{14900
*      here to exit with result on stack
{slpd3{lcw{7,xr{{{load next code word{14904
{{bri{9,(xr){{{execute it{14905
*      here if 2nd arg is negative or large
{slpd4{zer{8,wc{{{zero pad count{14909
{{brn{6,slpd1{{{merge{14910
{{ejc{{{{{14911
*      lt
{s_ltf{ent{{{{entry point{14915
{{jsr{6,acomp{{{call arithmetic comparison routine{14916
{{err{1,147{26,lt first argument is not numeric{{{14917
{{err{1,148{26,lt second argument is not numeric{{{14918
{{ppm{6,exnul{{{return null if lt{14919
{{ppm{6,exfal{{{fail if eq{14920
{{ppm{6,exfal{{{fail if gt{14921
{{ejc{{{{{14922
*      ne
{s_nef{ent{{{{entry point{14926
{{jsr{6,acomp{{{call arithmetic comparison routine{14927
{{err{1,149{26,ne first argument is not numeric{{{14928
{{err{1,150{26,ne second argument is not numeric{{{14929
{{ppm{6,exnul{{{return null if lt{14930
{{ppm{6,exfal{{{fail if eq{14931
{{ppm{6,exnul{{{return null if gt{14932
{{ejc{{{{{14933
*      notany
{s_nay{ent{{{{entry point{14937
{{mov{8,wb{22,=p_nas{{set pcode for single char arg{14938
{{mov{7,xl{22,=p_nay{{pcode for multi-char arg{14939
{{mov{8,wc{22,=p_nad{{set pcode for expr arg{14940
{{jsr{6,patst{{{call common routine to build node{14941
{{err{1,151{26,notany argument is not a string or expression{{{14942
{{mov{11,-(xs){7,xr{{stack result{14943
{{lcw{7,xr{{{get next code word{14944
{{bri{9,(xr){{{execute it{14945
{{ejc{{{{{14946
*      opsyn
{s_ops{ent{{{{entry point{14950
{{jsr{6,gtsmi{{{load third argument{14951
{{err{1,152{26,opsyn third argument is not integer{{{14952
{{err{1,153{26,opsyn third argument is negative or too large{{{14953
{{mov{8,wb{8,wc{{if ok, save third argumnet{14954
{{mov{7,xr{10,(xs)+{{load second argument{14955
{{jsr{6,gtnvr{{{locate variable block{14956
{{err{1,154{26,opsyn second arg is not natural variable name{{{14957
{{mov{7,xl{13,vrfnc(xr){{if ok, load function block pointer{14958
{{bnz{8,wb{6,sops2{{jump if operator opsyn case{14959
*      here for function opsyn (third arg zero)
{{mov{7,xr{10,(xs)+{{load first argument{14963
{{jsr{6,gtnvr{{{get vrblk pointer{14964
{{err{1,155{26,opsyn first arg is not natural variable name{{{14965
*      merge here to perform function definition
{sops1{jsr{6,dffnc{{{call function definer{14969
{{brn{6,exnul{{{exit with null result{14970
*      here for operator opsyn (third arg non-zero)
{sops2{jsr{6,gtstg{{{get operator name{14974
{{ppm{6,sops5{{{jump if not string{14975
{{bne{8,wa{18,=num01{6,sops5{error if not one char long{14976
{{plc{7,xr{{{else point to character{14977
{{lch{8,wc{9,(xr){{load character name{14978
{{ejc{{{{{14979
*      opsyn (continued)
*      now set to search for matching unary or binary operator
*      name as appropriate. note that there are =opbun undefined
*      binary operators and =opuun undefined unary operators.
{{mov{8,wa{20,=r_uub{{point to unop pointers in case{14987
{{mov{7,xr{21,=opnsu{{point to names of unary operators{14988
{{add{8,wb{18,=opbun{{add no. of undefined binary ops{14989
{{beq{8,wb{18,=opuun{6,sops3{jump if unop (third arg was 1){14990
{{mov{8,wa{20,=r_uba{{else point to binary operator ptrs{14991
{{mov{7,xr{21,=opsnb{{point to names of binary operators{14992
{{mov{8,wb{18,=opbun{{set number of undefined binops{14993
*      merge here to check list (wb = number to check)
{sops3{lct{8,wb{8,wb{{set counter to control loop{14997
*      loop to search for name match
{sops4{beq{8,wc{9,(xr){6,sops6{jump if names match{15001
{{ica{8,wa{{{else push pointer to function ptr{15002
{{ica{7,xr{{{bump pointer{15003
{{bct{8,wb{6,sops4{{loop back till all checked{15004
*      here if bad operator name
{sops5{erb{1,156{26,opsyn first arg is not correct operator name{{{15008
*      come here on finding a match in the operator name table
{sops6{mov{7,xr{8,wa{{copy pointer to function block ptr{15012
{{sub{7,xr{19,*vrfnc{{make it look like dummy vrblk{15013
{{brn{6,sops1{{{merge back to define operator{15014
{{ejc{{{{{15015
*      output
{s_oup{ent{{{{entry point{15040
{{mov{8,wb{18,=num03{{output flag{15041
{{jsr{6,ioput{{{call input/output assoc. routine{15042
{{err{1,157{26,output third argument is not a string{{{15043
{{err{1,158{26,inappropriate second argument for output{{{15044
{{err{1,159{26,inappropriate first argument for output{{{15045
{{err{1,160{26,inappropriate file specification for output{{{15046
{{ppm{6,exfal{{{fail if file does not exist{15047
{{err{1,161{26,output file cannot be written to{{{15048
{{err{1,290{26,output channel currently in use{{{15049
{{brn{6,exnul{{{return null string{15050
{{ejc{{{{{15051
*      pos
{s_pos{ent{{{{entry point{15055
{{mov{8,wb{22,=p_pos{{set pcode for integer arg case{15056
{{mov{8,wa{22,=p_psd{{set pcode for expression arg case{15057
{{jsr{6,patin{{{call common routine to build node{15058
{{err{1,162{26,pos argument is not integer or expression{{{15059
{{err{1,163{26,pos argument is negative or too large{{{15060
{{mov{11,-(xs){7,xr{{stack result{15061
{{lcw{7,xr{{{get next code word{15062
{{bri{9,(xr){{{execute it{15063
{{ejc{{{{{15064
*      prototype
{s_pro{ent{{{{entry point{15068
{{mov{7,xr{10,(xs)+{{load argument{15069
{{mov{8,wb{13,tblen(xr){{length if table, vector (=vclen){15070
{{btw{8,wb{{{convert to words{15071
{{mov{8,wa{9,(xr){{load type word of argument block{15072
{{beq{8,wa{22,=b_art{6,spro4{jump if array{15073
{{beq{8,wa{22,=b_tbt{6,spro1{jump if table{15074
{{beq{8,wa{22,=b_vct{6,spro3{jump if vector{15075
{{erb{1,164{26,prototype argument is not valid object{{{15080
*      here for table
{spro1{sub{8,wb{18,=tbsi_{{subtract standard fields{15084
*      merge for vector
{spro2{mti{8,wb{{{convert to integer{15088
{{brn{6,exint{{{exit with integer result{15089
*      here for vector
{spro3{sub{8,wb{18,=vcsi_{{subtract standard fields{15093
{{brn{6,spro2{{{merge{15094
*      here for array
{spro4{add{7,xr{13,arofs(xr){{point to prototype field{15098
{{mov{7,xr{9,(xr){{load prototype{15099
{{mov{11,-(xs){7,xr{{stack result{15100
{{lcw{7,xr{{{get next code word{15101
{{bri{9,(xr){{{execute it{15102
{{ejc{{{{{15112
*      remdr
{s_rmd{ent{{{{entry point{15116
{{jsr{6,arith{{{get two integers or two reals{15118
{{err{1,166{26,remdr first argument is not numeric{{{15119
{{err{1,165{26,remdr second argument is not numeric{{{15120
{{ppm{6,srm06{{{if real{15121
*      both arguments integer
{{zer{8,wb{{{set positive flag{15138
{{ldi{13,icval(xr){{{load left argument value{15139
{{ige{6,srm01{{{jump if positive{15140
{{mnz{8,wb{{{set negative flag{15141
{srm01{rmi{13,icval(xl){{{get remainder{15142
{{iov{6,srm05{{{error if overflow{15143
*      make sign of result match sign of first argument
{{bze{8,wb{6,srm03{{if result should be positive{15147
{{ile{6,exint{{{if should be negative, and is{15148
{srm02{ngi{{{{adjust sign of result{15149
{{brn{6,exint{{{return result{15150
{srm03{ilt{6,srm02{{{should be pos, and result negative{15151
{{brn{6,exint{{{should be positive, and is{15152
*      fail first argument
{srm04{erb{1,166{26,remdr first argument is not numeric{{{15156
*      fail if overflow
{srm05{erb{1,167{26,remdr caused integer overflow{{{15160
*      here with 1st argument in (xr), 2nd in (xl), both real
*      result = n1 - chop(n1/n2)*n2
{srm06{zer{8,wb{{{set positive flag{15167
{{ldr{13,rcval(xr){{{load left argument value{15168
{{rge{6,srm07{{{jump if positive{15169
{{mnz{8,wb{{{set negative flag{15170
{srm07{dvr{13,rcval(xl){{{compute n1/n2{15171
{{rov{6,srm10{{{jump if overflow{15172
{{chp{{{{chop result{15173
{{mlr{13,rcval(xl){{{times n2{15174
{{sbr{13,rcval(xr){{{compute difference{15175
*      make sign of result match sign of first argument
*      -result is in ra at this point
{{bze{8,wb{6,srm09{{if result should be positive{15180
{{rle{6,exrea{{{if should be negative, and is{15181
{srm08{ngr{{{{adjust sign of result{15182
{{brn{6,exrea{{{return result{15183
{srm09{rlt{6,srm08{{{should be pos, and result negative{15184
{{brn{6,exrea{{{should be positive, and is{15185
*      fail if overflow
{srm10{erb{1,312{26,remdr caused real overflow{{{15189
{{ejc{{{{{15191
*      replace
*      the actual replace operation uses an scblk whose cfp_a
*      chars contain the translated versions of all the chars.
*      the table pointer is remembered from call to call and
*      the table is only built when the arguments change.
*      we also perform an optimization gleaned from spitbol 370.
*      if the second argument is &alphabet, there is no need to
*      to build a replace table.  the third argument can be
*      used directly as the replace table.
{s_rpl{ent{{{{entry point{15205
{{jsr{6,gtstg{{{load third argument as string{15206
{{err{1,168{26,replace third argument is not a string{{{15207
{{mov{7,xl{7,xr{{save third arg ptr{15208
{{jsr{6,gtstg{{{get second argument{15209
{{err{1,169{26,replace second argument is not a string{{{15210
*      check to see if this is the same table as last time
{{bne{7,xr{3,r_ra2{6,srpl1{jump if 2nd argument different{15214
{{beq{7,xl{3,r_ra3{6,srpl4{jump if args same as last time{15215
*      here we build a new replace table (note wa = 2nd arg len)
{srpl1{mov{8,wb{13,sclen(xl){{load 3rd argument length{15219
{{bne{8,wa{8,wb{6,srpl6{jump if arguments not same length{15220
{{beq{7,xr{3,kvalp{6,srpl5{jump if 2nd arg is alphabet string{15221
{{bze{8,wb{6,srpl6{{jump if null 2nd argument{15222
{{mov{3,r_ra3{7,xl{{save third arg for next time in{15223
{{mov{3,r_ra2{7,xr{{save second arg for next time in{15224
{{mov{7,xl{3,kvalp{{point to alphabet string{15225
{{mov{8,wa{13,sclen(xl){{load alphabet scblk length{15226
{{mov{7,xr{3,r_rpt{{point to current table (if any){15227
{{bnz{7,xr{6,srpl2{{jump if we already have a table{15228
*      here we allocate a new table
{{jsr{6,alocs{{{allocate new table{15232
{{mov{8,wa{8,wc{{keep scblk length{15233
{{mov{3,r_rpt{7,xr{{save table pointer for next time{15234
*      merge here with pointer to new table block in (xr)
{srpl2{ctb{8,wa{2,scsi_{{compute length of scblk{15238
{{mvw{{{{copy to get initial table values{15239
{{ejc{{{{{15240
*      replace (continued)
*      now we must plug selected entries as required. note that
*      we are short of index registers for the following loop.
*      hence the need to repeatedly re-initialise char ptr xl
{{mov{7,xl{3,r_ra2{{point to second argument{15248
{{lct{8,wb{8,wb{{number of chars to plug{15249
{{zer{8,wc{{{zero char offset{15250
{{mov{7,xr{3,r_ra3{{point to 3rd arg{15251
{{plc{7,xr{{{get char ptr for 3rd arg{15252
*      loop to plug chars
{srpl3{mov{7,xl{3,r_ra2{{point to 2nd arg{15256
{{plc{7,xl{8,wc{{point to next char{15257
{{icv{8,wc{{{increment offset{15258
{{lch{8,wa{9,(xl){{get next char{15259
{{mov{7,xl{3,r_rpt{{point to translate table{15260
{{psc{7,xl{8,wa{{convert char to offset into table{15261
{{lch{8,wa{10,(xr)+{{get translated char{15262
{{sch{8,wa{9,(xl){{store in table{15263
{{csc{7,xl{{{complete store characters{15264
{{bct{8,wb{6,srpl3{{loop till done{15265
{{ejc{{{{{15266
*      replace (continued)
*      here to use r_rpt as replace table.
{srpl4{mov{7,xl{3,r_rpt{{replace table to use{15272
*      here to perform translate using table in xl.
{srpl5{jsr{6,gtstg{{{get first argument{15277
{{err{1,170{26,replace first argument is not a string{{{15278
{{bze{8,wa{6,exnul{{return null if null argument{15287
{{mov{11,-(xs){7,xl{{stack replace table to use{15288
{{mov{7,xl{7,xr{{copy pointer{15289
{{mov{8,wc{8,wa{{save length{15290
{{ctb{8,wa{2,schar{{get scblk length{15291
{{jsr{6,alloc{{{allocate space for copy{15292
{{mov{8,wb{7,xr{{save address of copy{15293
{{mvw{{{{move scblk contents to copy{15294
{{mov{7,xr{10,(xs)+{{unstack replace table{15295
{{plc{7,xr{{{point to chars of table{15296
{{mov{7,xl{8,wb{{point to string to translate{15297
{{plc{7,xl{{{point to chars of string{15298
{{mov{8,wa{8,wc{{set number of chars to translate{15299
{{trc{{{{perform translation{15300
{srpl8{mov{11,-(xs){8,wb{{stack result{15301
{{lcw{7,xr{{{load next code word{15302
{{bri{9,(xr){{{execute it{15303
*      error point
{srpl6{erb{1,171{26,null or unequally long 2nd, 3rd args to replace{{{15307
{{ejc{{{{{15322
*      rewind
{s_rew{ent{{{{entry point{15326
{{jsr{6,iofcb{{{call fcblk routine{15327
{{err{1,172{26,rewind argument is not a suitable name{{{15328
{{err{1,173{26,rewind argument is null{{{15329
{{err{1,174{26,rewind file does not exist{{{15330
{{jsr{6,sysrw{{{call system rewind function{15331
{{err{1,174{26,rewind file does not exist{{{15332
{{err{1,175{26,rewind file does not permit rewind{{{15333
{{err{1,176{26,rewind caused non-recoverable error{{{15334
{{brn{6,exnul{{{exit with null result if no error{15335
{{ejc{{{{{15336
*      reverse
{s_rvs{ent{{{{entry point{15340
{{jsr{6,gtstg{{{load string argument{15342
{{err{1,177{26,reverse argument is not a string{{{15343
{{bze{8,wa{6,exixr{{return argument if null{15349
{{mov{7,xl{7,xr{{else save pointer to string arg{15350
{{jsr{6,alocs{{{allocate space for new scblk{15351
{{mov{11,-(xs){7,xr{{store scblk ptr on stack as result{15352
{{psc{7,xr{{{prepare to store in new scblk{15353
{{plc{7,xl{8,wc{{point past last char in argument{15354
{{lct{8,wc{8,wc{{set loop counter{15355
*      loop to move chars in reverse order
{srvs1{lch{8,wb{11,-(xl){{load next char from argument{15359
{{sch{8,wb{10,(xr)+{{store in result{15360
{{bct{8,wc{6,srvs1{{loop till all moved{15361
*      here when complete to execute next code word
{srvs4{csc{7,xr{{{complete store characters{15365
{{zer{7,xl{{{clear garbage xl{15366
{srvs2{lcw{7,xr{{{load next code word{15367
{{bri{9,(xr){{{execute it{15368
{{ejc{{{{{15392
*      rpad
{s_rpd{ent{{{{entry point{15396
{{jsr{6,gtstg{{{get pad character{15397
{{err{1,178{26,rpad third argument is not a string{{{15398
{{plc{7,xr{{{point to character (null is blank){15399
{{lch{8,wb{9,(xr){{load pad character{15400
{{jsr{6,gtsmi{{{get pad length{15401
{{err{1,179{26,rpad second argument is not integer{{{15402
{{ppm{6,srpd3{{{skip if negative or large{15403
*      merge to check first arg.
{srpd1{jsr{6,gtstg{{{get first argument (string to pad){15407
{{err{1,180{26,rpad first argument is not a string{{{15408
{{bge{8,wa{8,wc{6,exixr{return 1st arg if too long to pad{15409
{{mov{7,xl{7,xr{{else move ptr to string to pad{15410
*      now we are ready for the pad
*      (xl)                  pointer to string to pad
*      (wb)                  pad character
*      (wc)                  length to pad string to
{{mov{8,wa{8,wc{{copy length{15418
{{jsr{6,alocs{{{allocate scblk for new string{15419
{{mov{11,-(xs){7,xr{{save as result{15420
{{mov{8,wa{13,sclen(xl){{load length of argument{15421
{{sub{8,wc{8,wa{{calculate number of pad characters{15422
{{psc{7,xr{{{point to chars in result string{15423
{{lct{8,wc{8,wc{{set counter for pad loop{15424
*      copy argument string
{{bze{8,wa{6,srpd2{{jump if argument is null{15428
{{plc{7,xl{{{else point to argument chars{15429
{{mvc{{{{move characters to result string{15430
{{zer{7,xl{{{clear garbage xl{15431
*      loop to supply pad characters
{srpd2{sch{8,wb{10,(xr)+{{store pad character, bump ptr{15435
{{bct{8,wc{6,srpd2{{loop till all pad chars stored{15436
{{csc{7,xr{{{complete character storing{15437
{{lcw{7,xr{{{load next code word{15438
{{bri{9,(xr){{{execute it{15439
*      here if 2nd arg is negative or large
{srpd3{zer{8,wc{{{zero pad count{15443
{{brn{6,srpd1{{{merge{15444
{{ejc{{{{{15445
*      rtab
{s_rtb{ent{{{{entry point{15449
{{mov{8,wb{22,=p_rtb{{set pcode for integer arg case{15450
{{mov{8,wa{22,=p_rtd{{set pcode for expression arg case{15451
{{jsr{6,patin{{{call common routine to build node{15452
{{err{1,181{26,rtab argument is not integer or expression{{{15453
{{err{1,182{26,rtab argument is negative or too large{{{15454
{{mov{11,-(xs){7,xr{{stack result{15455
{{lcw{7,xr{{{get next code word{15456
{{bri{9,(xr){{{execute it{15457
{{ejc{{{{{15458
*      tab
{s_tab{ent{{{{entry point{15499
{{mov{8,wb{22,=p_tab{{set pcode for integer arg case{15500
{{mov{8,wa{22,=p_tbd{{set pcode for expression arg case{15501
{{jsr{6,patin{{{call common routine to build node{15502
{{err{1,183{26,tab argument is not integer or expression{{{15503
{{err{1,184{26,tab argument is negative or too large{{{15504
{{mov{11,-(xs){7,xr{{stack result{15505
{{lcw{7,xr{{{get next code word{15506
{{bri{9,(xr){{{execute it{15507
{{ejc{{{{{15508
*      rpos
{s_rps{ent{{{{entry point{15512
{{mov{8,wb{22,=p_rps{{set pcode for integer arg case{15513
{{mov{8,wa{22,=p_rpd{{set pcode for expression arg case{15514
{{jsr{6,patin{{{call common routine to build node{15515
{{err{1,185{26,rpos argument is not integer or expression{{{15516
{{err{1,186{26,rpos argument is negative or too large{{{15517
{{mov{11,-(xs){7,xr{{stack result{15518
{{lcw{7,xr{{{get next code word{15519
{{bri{9,(xr){{{execute it{15520
{{ejc{{{{{15523
*      rsort
{s_rsr{ent{{{{entry point{15527
{{mnz{8,wa{{{mark as rsort{15528
{{jsr{6,sorta{{{call sort routine{15529
{{ppm{6,exfal{{{if conversion fails, so shall we{15530
{{brn{6,exsid{{{return, setting idval{15531
{{ejc{{{{{15533
*      setexit
{s_stx{ent{{{{entry point{15537
{{mov{7,xr{10,(xs)+{{load argument{15538
{{mov{8,wa{3,stxvr{{load old vrblk pointer{15539
{{zer{7,xl{{{load zero in case null arg{15540
{{beq{7,xr{21,=nulls{6,sstx1{jump if null argument (reset call){15541
{{jsr{6,gtnvr{{{else get specified vrblk{15542
{{ppm{6,sstx2{{{jump if not natural variable{15543
{{mov{7,xl{13,vrlbl(xr){{else load label{15544
{{beq{7,xl{21,=stndl{6,sstx2{jump if label is not defined{15545
{{bne{9,(xl){22,=b_trt{6,sstx1{jump if not trapped{15546
{{mov{7,xl{13,trlbl(xl){{else load ptr to real label code{15547
*      here to set/reset setexit trap
{sstx1{mov{3,stxvr{7,xr{{store new vrblk pointer (or null){15551
{{mov{3,r_sxc{7,xl{{store new code ptr (or zero){15552
{{beq{8,wa{21,=nulls{6,exnul{return null if null result{15553
{{mov{7,xr{8,wa{{else copy vrblk pointer{15554
{{brn{6,exvnm{{{and return building nmblk{15555
*      here if bad argument
{sstx2{erb{1,187{26,setexit argument is not label name or null{{{15559
*      sin
{s_sin{ent{{{{entry point{15564
{{mov{7,xr{10,(xs)+{{get argument{15565
{{jsr{6,gtrea{{{convert to real{15566
{{err{1,308{26,sin argument not numeric{{{15567
{{ldr{13,rcval(xr){{{load accumulator with argument{15568
{{sin{{{{take sine{15569
{{rno{6,exrea{{{if no overflow, return result in ra{15570
{{erb{1,323{26,sin argument is out of range{{{15571
{{ejc{{{{{15572
*      sqrt
{s_sqr{ent{{{{entry point{15578
{{mov{7,xr{10,(xs)+{{get argument{15579
{{jsr{6,gtrea{{{convert to real{15580
{{err{1,313{26,sqrt argument not numeric{{{15581
{{ldr{13,rcval(xr){{{load accumulator with argument{15582
{{rlt{6,ssqr1{{{negative number{15583
{{sqr{{{{take square root{15584
{{brn{6,exrea{{{no overflow possible, result in ra{15585
*      here if bad argument
{ssqr1{erb{1,314{26,sqrt argument negative{{{15589
{{ejc{{{{{15590
{{ejc{{{{{15594
*      sort
{s_srt{ent{{{{entry point{15598
{{zer{8,wa{{{mark as sort{15599
{{jsr{6,sorta{{{call sort routine{15600
{{ppm{6,exfal{{{if conversion fails, so shall we{15601
{{brn{6,exsid{{{return, setting idval{15602
{{ejc{{{{{15604
*      span
{s_spn{ent{{{{entry point{15608
{{mov{8,wb{22,=p_sps{{set pcode for single char arg{15609
{{mov{7,xl{22,=p_spn{{set pcode for multi-char arg{15610
{{mov{8,wc{22,=p_spd{{set pcode for expression arg{15611
{{jsr{6,patst{{{call common routine to build node{15612
{{err{1,188{26,span argument is not a string or expression{{{15613
{{mov{11,-(xs){7,xr{{stack result{15614
{{lcw{7,xr{{{get next code word{15615
{{bri{9,(xr){{{execute it{15616
{{ejc{{{{{15617
*      size
{s_si_{ent{{{{entry point{15621
{{jsr{6,gtstg{{{load string argument{15623
{{err{1,189{26,size argument is not a string{{{15624
*      merge with bfblk or scblk ptr in xr.  wa has length.
{{mti{8,wa{{{load length as integer{15632
{{brn{6,exint{{{exit with integer result{15633
{{ejc{{{{{15634
*      stoptr
{s_stt{ent{{{{entry point{15638
{{zer{7,xl{{{indicate stoptr case{15639
{{jsr{6,trace{{{call trace procedure{15640
{{err{1,190{26,stoptr first argument is not appropriate name{{{15641
{{err{1,191{26,stoptr second argument is not trace type{{{15642
{{brn{6,exnul{{{return null{15643
{{ejc{{{{{15644
*      substr
{s_sub{ent{{{{entry point{15648
{{jsr{6,gtsmi{{{load third argument{15649
{{err{1,192{26,substr third argument is not integer{{{15650
{{ppm{6,exfal{{{jump if negative or too large{15651
{{mov{3,sbssv{7,xr{{save third argument{15652
{{jsr{6,gtsmi{{{load second argument{15653
{{err{1,193{26,substr second argument is not integer{{{15654
{{ppm{6,exfal{{{jump if out of range{15655
{{mov{8,wc{7,xr{{save second argument{15656
{{bze{8,wc{6,exfal{{jump if second argument zero{15657
{{dcv{8,wc{{{else decrement for ones origin{15658
{{jsr{6,gtstg{{{load first argument{15660
{{err{1,194{26,substr first argument is not a string{{{15661
*      merge with bfblk or scblk ptr in xr.  wa has length
{{mov{8,wb{8,wc{{copy second arg to wb{15669
{{mov{8,wc{3,sbssv{{reload third argument{15670
{{bnz{8,wc{6,ssub2{{skip if third arg given{15671
{{mov{8,wc{8,wa{{else get string length{15672
{{bgt{8,wb{8,wc{6,exfal{fail if improper{15673
{{sub{8,wc{8,wb{{reduce by offset to start{15674
*      merge
{ssub2{mov{7,xl{8,wa{{save string length{15678
{{mov{8,wa{8,wc{{set length of substring{15679
{{add{8,wc{8,wb{{add 2nd arg to 3rd arg{15680
{{bgt{8,wc{7,xl{6,exfal{jump if improper substring{15681
{{mov{7,xl{7,xr{{copy pointer to first arg{15682
{{jsr{6,sbstr{{{build substring{15683
{{mov{11,-(xs){7,xr{{stack result{15684
{{lcw{7,xr{{{get next code word{15685
{{bri{9,(xr){{{execute it{15686
{{ejc{{{{{15687
*      table
{s_tbl{ent{{{{entry point{15691
{{mov{7,xl{10,(xs)+{{get initial lookup value{15692
{{ica{7,xs{{{pop second argument{15693
{{jsr{6,gtsmi{{{load argument{15694
{{err{1,195{26,table argument is not integer{{{15695
{{err{1,196{26,table argument is out of range{{{15696
{{bnz{8,wc{6,stbl1{{jump if non-zero{15697
{{mov{8,wc{18,=tbnbk{{else supply default value{15698
*      merge here with number of headers in wc
{stbl1{jsr{6,tmake{{{make table{15702
{{brn{6,exsid{{{exit setting idval{15703
{{ejc{{{{{15704
*      tan
{s_tan{ent{{{{entry point{15709
{{mov{7,xr{10,(xs)+{{get argument{15710
{{jsr{6,gtrea{{{convert to real{15711
{{err{1,309{26,tan argument not numeric{{{15712
{{ldr{13,rcval(xr){{{load accumulator with argument{15713
{{tan{{{{take tangent{15714
{{rno{6,exrea{{{if no overflow, return result in ra{15715
{{erb{1,310{26,tan produced real overflow or argument is out of range{{{15716
{{ejc{{{{{15717
*      time
{s_tim{ent{{{{entry point{15722
{{jsr{6,systm{{{get timer value{15723
{{sbi{3,timsx{{{subtract starting time{15724
{{brn{6,exint{{{exit with integer value{15725
{{ejc{{{{{15726
*      trace
{s_tra{ent{{{{entry point{15730
{{beq{13,num03(xs){21,=nulls{6,str02{jump if first argument is null{15731
{{mov{7,xr{10,(xs)+{{load fourth argument{15732
{{zer{7,xl{{{tentatively set zero pointer{15733
{{beq{7,xr{21,=nulls{6,str01{jump if 4th argument is null{15734
{{jsr{6,gtnvr{{{else point to vrblk{15735
{{ppm{6,str03{{{jump if not variable name{15736
{{mov{7,xl{7,xr{{else save vrblk in trfnc{15737
*      here with vrblk or zero in xl
{str01{mov{7,xr{10,(xs)+{{load third argument (tag){15741
{{zer{8,wb{{{set zero as trtyp value for now{15742
{{jsr{6,trbld{{{build trblk for trace call{15743
{{mov{7,xl{7,xr{{move trblk pointer for trace{15744
{{jsr{6,trace{{{call trace procedure{15745
{{err{1,198{26,trace first argument is not appropriate name{{{15746
{{err{1,199{26,trace second argument is not trace type{{{15747
{{brn{6,exnul{{{return null{15748
*      here to call system trace toggle routine
{str02{jsr{6,systt{{{call it{15752
{{add{7,xs{19,*num04{{pop trace arguments{15753
{{brn{6,exnul{{{return{15754
*      here for bad fourth argument
{str03{erb{1,197{26,trace fourth arg is not function name or null{{{15758
{{ejc{{{{{15759
*      trim
{s_trm{ent{{{{entry point{15763
{{jsr{6,gtstg{{{load argument as string{15765
{{err{1,200{26,trim argument is not a string{{{15766
{{bze{8,wa{6,exnul{{return null if argument is null{15772
{{mov{7,xl{7,xr{{copy string pointer{15773
{{ctb{8,wa{2,schar{{get block length{15774
{{jsr{6,alloc{{{allocate copy same size{15775
{{mov{8,wb{7,xr{{save pointer to copy{15776
{{mvw{{{{copy old string block to new{15777
{{mov{7,xr{8,wb{{restore ptr to new block{15778
{{jsr{6,trimr{{{trim blanks (wb is non-zero){15779
{{mov{11,-(xs){7,xr{{stack result{15780
{{lcw{7,xr{{{get next code word{15781
{{bri{9,(xr){{{execute it{15782
{{ejc{{{{{15825
*      unload
{s_unl{ent{{{{entry point{15829
{{mov{7,xr{10,(xs)+{{load argument{15830
{{jsr{6,gtnvr{{{point to vrblk{15831
{{err{1,201{26,unload argument is not natural variable name{{{15832
{{mov{7,xl{21,=stndf{{get ptr to undefined function{15833
{{jsr{6,dffnc{{{undefine named function{15834
{{brn{6,exnul{{{return null as result{15835
{{ttl{27,s p i t b o l -- utility routines{{{{15857
*      the following section contains utility routines used for
*      various purposes throughout the system. these differ
*      from the procedures in the utility procedures section in
*      they are not in procedure form and they do not return
*      to their callers. they are accessed with a branch type
*      instruction after setting the registers to appropriate
*      parameter values.
*      the register values required for each routine are
*      documented at the start of each routine. registers not
*      mentioned may contain any values except that xr,xl
*      can only contain proper collectable pointers.
*      some of these routines will tolerate garbage pointers
*      in xl,xr on entry. this is always documented and in
*      each case, the routine clears these garbage values before
*      exiting after completing its task.
*      the routines have names consisting of five letters
*      and are assembled in alphabetical order.
{{ejc{{{{{15879
*      arref -- array reference
*      (xl)                  may be non-collectable
*      (xr)                  number of subscripts
*      (wb)                  set zero/nonzero for value/name
*                            the value in wb must be collectable
*      stack                 subscripts and array operand
*      brn  arref            jump to call function
*      arref continues by executing the next code word with
*      the result name or value placed on top of the stack.
*      to deal with the problem of accessing subscripts in the
*      order of stacking, xl is used as a subscript pointer
*      working below the stack pointer.
{arref{rtn{{{{{15895
{{mov{8,wa{7,xr{{copy number of subscripts{15896
{{mov{7,xt{7,xs{{point to stack front{15897
{{wtb{7,xr{{{convert to byte offset{15898
{{add{7,xt{7,xr{{point to array operand on stack{15899
{{ica{7,xt{{{final value for stack popping{15900
{{mov{3,arfxs{7,xt{{keep for later{15901
{{mov{7,xr{11,-(xt){{load array operand pointer{15902
{{mov{3,r_arf{7,xr{{keep array pointer{15903
{{mov{7,xr{7,xt{{save pointer to subscripts{15904
{{mov{7,xl{3,r_arf{{point xl to possible vcblk or tbblk{15905
{{mov{8,wc{9,(xl){{load first word{15906
{{beq{8,wc{22,=b_art{6,arf01{jump if arblk{15907
{{beq{8,wc{22,=b_vct{6,arf07{jump if vcblk{15908
{{beq{8,wc{22,=b_tbt{6,arf10{jump if tbblk{15909
{{erb{1,235{26,subscripted operand is not table or array{{{15910
*      here for array (arblk)
{arf01{bne{8,wa{13,arndm(xl){6,arf09{jump if wrong number of dims{15914
{{ldi{4,intv0{{{get initial subscript of zero{15915
{{mov{7,xt{7,xr{{point before subscripts{15916
{{zer{8,wa{{{initial offset to bounds{15917
{{brn{6,arf03{{{jump into loop{15918
*      loop to compute subscripts by multiplications
{arf02{mli{13,ardm2(xr){{{multiply total by next dimension{15922
*      merge here first time
{arf03{mov{7,xr{11,-(xt){{load next subscript{15926
{{sti{3,arfsi{{{save current subscript{15927
{{ldi{13,icval(xr){{{load integer value in case{15928
{{beq{9,(xr){22,=b_icl{6,arf04{jump if it was an integer{15929
{{ejc{{{{{15930
*      arref (continued)
{{jsr{6,gtint{{{convert to integer{15935
{{ppm{6,arf12{{{jump if not integer{15936
{{ldi{13,icval(xr){{{if ok, load integer value{15937
*      here with integer subscript in (ia)
{arf04{mov{7,xr{3,r_arf{{point to array{15941
{{add{7,xr{8,wa{{offset to next bounds{15942
{{sbi{13,arlbd(xr){{{subtract low bound to compare{15943
{{iov{6,arf13{{{out of range fail if overflow{15944
{{ilt{6,arf13{{{out of range fail if too small{15945
{{sbi{13,ardim(xr){{{subtract dimension{15946
{{ige{6,arf13{{{out of range fail if too large{15947
{{adi{13,ardim(xr){{{else restore subscript offset{15948
{{adi{3,arfsi{{{add to current total{15949
{{add{8,wa{19,*ardms{{point to next bounds{15950
{{bne{7,xt{7,xs{6,arf02{loop back if more to go{15951
*      here with integer subscript computed
{{mfi{8,wa{{{get as one word integer{15955
{{wtb{8,wa{{{convert to offset{15956
{{mov{7,xl{3,r_arf{{point to arblk{15957
{{add{8,wa{13,arofs(xl){{add offset past bounds{15958
{{ica{8,wa{{{adjust for arpro field{15959
{{bnz{8,wb{6,arf08{{exit with name if name call{15960
*      merge here to get value for value call
{arf05{jsr{6,acess{{{get value{15964
{{ppm{6,arf13{{{fail if acess fails{15965
*      return value
{arf06{mov{7,xs{3,arfxs{{pop stack entries{15969
{{zer{3,r_arf{{{finished with array pointer{15970
{{mov{11,-(xs){7,xr{{stack result{15971
{{lcw{7,xr{{{get next code word{15972
{{bri{9,(xr){{{execute it{15973
{{ejc{{{{{15974
*      arref (continued)
*      here for vector
{arf07{bne{8,wa{18,=num01{6,arf09{error if more than 1 subscript{15980
{{mov{7,xr{9,(xs){{else load subscript{15981
{{jsr{6,gtint{{{convert to integer{15982
{{ppm{6,arf12{{{error if not integer{15983
{{ldi{13,icval(xr){{{else load integer value{15984
{{sbi{4,intv1{{{subtract for ones offset{15985
{{mfi{8,wa{6,arf13{{get subscript as one word{15986
{{add{8,wa{18,=vcvls{{add offset for standard fields{15987
{{wtb{8,wa{{{convert offset to bytes{15988
{{bge{8,wa{13,vclen(xl){6,arf13{fail if out of range subscript{15989
{{bze{8,wb{6,arf05{{back to get value if value call{15990
*      return name
{arf08{mov{7,xs{3,arfxs{{pop stack entries{15994
{{zer{3,r_arf{{{finished with array pointer{15995
{{brn{6,exnam{{{else exit with name{15996
*      here if subscript count is wrong
{arf09{erb{1,236{26,array referenced with wrong number of subscripts{{{16000
*      table
{arf10{bne{8,wa{18,=num01{6,arf11{error if more than 1 subscript{16004
{{mov{7,xr{9,(xs){{else load subscript{16005
{{jsr{6,tfind{{{call table search routine{16006
{{ppm{6,arf13{{{fail if failed{16007
{{bnz{8,wb{6,arf08{{exit with name if name call{16008
{{brn{6,arf06{{{else exit with value{16009
*      here for bad table reference
{arf11{erb{1,237{26,table referenced with more than one subscript{{{16013
*      here for bad subscript
{arf12{erb{1,238{26,array subscript is not integer{{{16017
*      here to signal failure
{arf13{zer{3,r_arf{{{finished with array pointer{16021
{{brn{6,exfal{{{fail{16022
{{ejc{{{{{16023
*      cfunc -- call a function
*      cfunc is used to call a snobol level function. it is
*      used by the apply function (s_app), the function
*      trace routine (trxeq) and the main function call entry
*      (o_fnc, o_fns). in the latter cases, cfunc is used only
*      if the number of arguments is incorrect.
*      (xl)                  pointer to function block
*      (wa)                  actual number of arguments
*      (xs)                  points to stacked arguments
*      brn  cfunc            jump to call function
*      cfunc continues by executing the function
{cfunc{rtn{{{{{16040
{{blt{8,wa{13,fargs(xl){6,cfnc1{jump if too few arguments{16041
{{beq{8,wa{13,fargs(xl){6,cfnc3{jump if correct number of args{16042
*      here if too many arguments supplied, pop them off
{{mov{8,wb{8,wa{{copy actual number{16046
{{sub{8,wb{13,fargs(xl){{get number of extra args{16047
{{wtb{8,wb{{{convert to bytes{16048
{{add{7,xs{8,wb{{pop off unwanted arguments{16049
{{brn{6,cfnc3{{{jump to go off to function{16050
*      here if too few arguments
{cfnc1{mov{8,wb{13,fargs(xl){{load required number of arguments{16054
{{beq{8,wb{18,=nini9{6,cfnc3{jump if case of var num of args{16055
{{sub{8,wb{8,wa{{calculate number missing{16056
{{lct{8,wb{8,wb{{set counter to control loop{16057
*      loop to supply extra null arguments
{cfnc2{mov{11,-(xs){21,=nulls{{stack a null argument{16061
{{bct{8,wb{6,cfnc2{{loop till proper number stacked{16062
*      merge here to jump to function
{cfnc3{bri{9,(xl){{{jump through fcode field{16066
{{ejc{{{{{16067
*      exfal -- exit signalling snobol failure
*      (xl,xr)               may be non-collectable
*      brn  exfal            jump to fail
*      exfal continues by executing the appropriate fail goto
{exfal{rtn{{{{{16076
{{mov{7,xs{3,flptr{{pop stack{16077
{{mov{7,xr{9,(xs){{load failure offset{16078
{{add{7,xr{3,r_cod{{point to failure code location{16079
{{lcp{7,xr{{{set code pointer{16080
{{lcw{7,xr{{{load next code word{16081
{{mov{7,xl{9,(xr){{load entry address{16082
{{bri{7,xl{{{jump to execute next code word{16083
{{ejc{{{{{16084
*      exint -- exit with integer result
*      (xl,xr)               may be non-collectable
*      (ia)                  integer value
*      brn  exint            jump to exit with integer
*      exint continues by executing the next code word
*      which it does by falling through to exixr
{exint{rtn{{{{{16095
{{zer{7,xl{{{clear dud value{16096
{{jsr{6,icbld{{{build icblk{16097
{{ejc{{{{{16098
*      exixr -- exit with result in (xr)
*      (xr)                  result
*      (xl)                  may be non-collectable
*      brn  exixr            jump to exit with result in (xr)
*      exixr continues by executing the next code word
*      which it does by falling through to exits.
{exixr{rtn{{{{{16107
{{mov{11,-(xs){7,xr{{stack result{16109
*      exits -- exit with result if any stacked
*      (xr,xl)               may be non-collectable
*      brn  exits            enter exits routine
{exits{rtn{{{{{16118
{{lcw{7,xr{{{load next code word{16119
{{mov{7,xl{9,(xr){{load entry address{16120
{{bri{7,xl{{{jump to execute next code word{16121
{{ejc{{{{{16122
*      exnam -- exit with name in (xl,wa)
*      (xl)                  name base
*      (wa)                  name offset
*      (xr)                  may be non-collectable
*      brn  exnam            jump to exit with name in (xl,wa)
*      exnam continues by executing the next code word
{exnam{rtn{{{{{16133
{{mov{11,-(xs){7,xl{{stack name base{16134
{{mov{11,-(xs){8,wa{{stack name offset{16135
{{lcw{7,xr{{{load next code word{16136
{{bri{9,(xr){{{execute it{16137
{{ejc{{{{{16138
*      exnul -- exit with null result
*      (xl,xr)               may be non-collectable
*      brn  exnul            jump to exit with null value
*      exnul continues by executing the next code word
{exnul{rtn{{{{{16147
{{mov{11,-(xs){21,=nulls{{stack null value{16148
{{lcw{7,xr{{{load next code word{16149
{{mov{7,xl{9,(xr){{load entry address{16150
{{bri{7,xl{{{jump to execute next code word{16151
{{ejc{{{{{16152
*      exrea -- exit with real result
*      (xl,xr)               may be non-collectable
*      (ra)                  real value
*      brn  exrea            jump to exit with real value
*      exrea continues by executing the next code word
{exrea{rtn{{{{{16164
{{zer{7,xl{{{clear dud value{16165
{{jsr{6,rcbld{{{build rcblk{16166
{{brn{6,exixr{{{jump to exit with result in xr{16167
{{ejc{{{{{16169
*      exsid -- exit setting id field
*      exsid is used to exit after building any of the following
*      blocks (arblk, tbblk, pdblk, vcblk). it sets the idval.
*      (xr)                  ptr to block with idval field
*      (xl)                  may be non-collectable
*      brn  exsid            jump to exit after setting id field
*      exsid continues by executing the next code word
{exsid{rtn{{{{{16182
{{mov{8,wa{3,curid{{load current id value{16183
{{bne{8,wa{3,mxint{6,exsi1{jump if no overflow{16184
{{zer{8,wa{{{else reset for wraparound{16185
*      here with old idval in wa
{exsi1{icv{8,wa{{{bump id value{16189
{{mov{3,curid{8,wa{{store for next time{16190
{{mov{13,idval(xr){8,wa{{store id value{16191
{{brn{6,exixr{{{exit with result in (xr){16192
{{ejc{{{{{16193
*      exvnm -- exit with name of variable
*      exvnm exits after stacking a value which is a nmblk
*      referencing the name of a given natural variable.
*      (xr)                  vrblk pointer
*      (xl)                  may be non-collectable
*      brn  exvnm            exit with vrblk pointer in xr
{exvnm{rtn{{{{{16204
{{mov{7,xl{7,xr{{copy name base pointer{16205
{{mov{8,wa{19,*nmsi_{{set size of nmblk{16206
{{jsr{6,alloc{{{allocate nmblk{16207
{{mov{9,(xr){22,=b_nml{{store type word{16208
{{mov{13,nmbas(xr){7,xl{{store name base{16209
{{mov{13,nmofs(xr){19,*vrval{{store name offset{16210
{{brn{6,exixr{{{exit with result in xr{16211
{{ejc{{{{{16212
*      flpop -- fail and pop in pattern matching
*      flpop pops the node and cursor on the stack and then
*      drops through into failp to cause pattern failure
*      (xl,xr)               may be non-collectable
*      brn  flpop            jump to fail and pop stack
{flpop{rtn{{{{{16222
{{add{7,xs{19,*num02{{pop two entries off stack{16223
{{ejc{{{{{16224
*      failp -- failure in matching pattern node
*      failp is used after failing to match a pattern node.
*      see pattern match routines for details of use.
*      (xl,xr)               may be non-collectable
*      brn  failp            signal failure to match
*      failp continues by matching an alternative from the stack
{failp{rtn{{{{{16236
{{mov{7,xr{10,(xs)+{{load alternative node pointer{16237
{{mov{8,wb{10,(xs)+{{restore old cursor{16238
{{mov{7,xl{9,(xr){{load pcode entry pointer{16239
{{bri{7,xl{{{jump to execute code for node{16240
{{ejc{{{{{16241
*      indir -- compute indirect reference
*      (wb)                  nonzero/zero for by name/value
*      brn  indir            jump to get indirect ref on stack
*      indir continues by executing the next code word
{indir{rtn{{{{{16250
{{mov{7,xr{10,(xs)+{{load argument{16251
{{beq{9,(xr){22,=b_nml{6,indr2{jump if a name{16252
{{jsr{6,gtnvr{{{else convert to variable{16253
{{err{1,239{26,indirection operand is not name{{{16254
{{bze{8,wb{6,indr1{{skip if by value{16255
{{mov{11,-(xs){7,xr{{else stack vrblk ptr{16256
{{mov{11,-(xs){19,*vrval{{stack name offset{16257
{{lcw{7,xr{{{load next code word{16258
{{mov{7,xl{9,(xr){{load entry address{16259
{{bri{7,xl{{{jump to execute next code word{16260
*      here to get value of natural variable
{indr1{bri{9,(xr){{{jump through vrget field of vrblk{16264
*      here if operand is a name
{indr2{mov{7,xl{13,nmbas(xr){{load name base{16268
{{mov{8,wa{13,nmofs(xr){{load name offset{16269
{{bnz{8,wb{6,exnam{{exit if called by name{16270
{{jsr{6,acess{{{else get value first{16271
{{ppm{6,exfal{{{fail if access fails{16272
{{brn{6,exixr{{{else return with value in xr{16273
{{ejc{{{{{16274
*      match -- initiate pattern match
*      (wb)                  match type code
*      brn  match            jump to initiate pattern match
*      match continues by executing the pattern match. see
*      pattern match routines (p_xxx) for full details.
{match{rtn{{{{{16284
{{mov{7,xr{10,(xs)+{{load pattern operand{16285
{{jsr{6,gtpat{{{convert to pattern{16286
{{err{1,240{26,pattern match right operand is not pattern{{{16287
{{mov{7,xl{7,xr{{if ok, save pattern pointer{16288
{{bnz{8,wb{6,mtch1{{jump if not match by name{16289
{{mov{8,wa{9,(xs){{else load name offset{16290
{{mov{11,-(xs){7,xl{{save pattern pointer{16291
{{mov{7,xl{13,num02(xs){{load name base{16292
{{jsr{6,acess{{{access subject value{16293
{{ppm{6,exfal{{{fail if access fails{16294
{{mov{7,xl{9,(xs){{restore pattern pointer{16295
{{mov{9,(xs){7,xr{{stack subject string val for merge{16296
{{zer{8,wb{{{restore type code{16297
*      merge here with subject value on stack
{mtch1{jsr{6,gtstg{{{convert subject to string{16302
{{err{1,241{26,pattern match left operand is not a string{{{16303
{{mov{11,-(xs){8,wb{{stack match type code{16304
{{mov{3,r_pms{7,xr{{if ok, store subject string pointer{16312
{{mov{3,pmssl{8,wa{{and length{16313
{{zer{11,-(xs){{{stack initial cursor (zero){16314
{{zer{8,wb{{{set initial cursor{16315
{{mov{3,pmhbs{7,xs{{set history stack base ptr{16316
{{zer{3,pmdfl{{{reset pattern assignment flag{16317
{{mov{7,xr{7,xl{{set initial node pointer{16318
{{bnz{3,kvanc{6,mtch2{{jump if anchored{16319
*      here for unanchored
{{mov{11,-(xs){7,xr{{stack initial node pointer{16323
{{mov{11,-(xs){21,=nduna{{stack pointer to anchor move node{16324
{{bri{9,(xr){{{start match of first node{16325
*      here in anchored mode
{mtch2{zer{11,-(xs){{{dummy cursor value{16329
{{mov{11,-(xs){21,=ndabo{{stack pointer to abort node{16330
{{bri{9,(xr){{{start match of first node{16331
{{ejc{{{{{16332
*      retrn -- return from function
*      (wa)                  string pointer for return type
*      brn  retrn            jump to return from (snobol) func
*      retrn continues by executing the code at the return point
*      the stack is cleaned of any garbage left by other
*      routines which may have altered flptr since function
*      entry by using flprt, reserved for use only by
*      function call and return.
{retrn{rtn{{{{{16345
{{bnz{3,kvfnc{6,rtn01{{jump if not level zero{16346
{{erb{1,242{26,function return from level zero{{{16347
*      here if not level zero return
{rtn01{mov{7,xs{3,flprt{{pop stack{16351
{{ica{7,xs{{{remove failure offset{16352
{{mov{7,xr{10,(xs)+{{pop pfblk pointer{16353
{{mov{3,flptr{10,(xs)+{{pop failure pointer{16354
{{mov{3,flprt{10,(xs)+{{pop old flprt{16355
{{mov{8,wb{10,(xs)+{{pop code pointer offset{16356
{{mov{8,wc{10,(xs)+{{pop old code block pointer{16357
{{add{8,wb{8,wc{{make old code pointer absolute{16358
{{lcp{8,wb{{{restore old code pointer{16359
{{mov{3,r_cod{8,wc{{restore old code block pointer{16360
{{dcv{3,kvfnc{{{decrement function level{16361
{{mov{8,wb{3,kvtra{{load trace{16362
{{add{8,wb{3,kvftr{{add ftrace{16363
{{bze{8,wb{6,rtn06{{jump if no tracing possible{16364
*      here if there may be a trace
{{mov{11,-(xs){8,wa{{save function return type{16368
{{mov{11,-(xs){7,xr{{save pfblk pointer{16369
{{mov{3,kvrtn{8,wa{{set rtntype for trace function{16370
{{mov{7,xl{3,r_fnc{{load fnclevel trblk ptr (if any){16371
{{jsr{6,ktrex{{{execute possible fnclevel trace{16372
{{mov{7,xl{13,pfvbl(xr){{load vrblk ptr (sgd13){16373
{{bze{3,kvtra{6,rtn02{{jump if trace is off{16374
{{mov{7,xr{13,pfrtr(xr){{else load return trace trblk ptr{16375
{{bze{7,xr{6,rtn02{{jump if not return traced{16376
{{dcv{3,kvtra{{{else decrement trace count{16377
{{bze{13,trfnc(xr){6,rtn03{{jump if print trace{16378
{{mov{8,wa{19,*vrval{{else set name offset{16379
{{mov{3,kvrtn{13,num01(xs){{make sure rtntype is set right{16380
{{jsr{6,trxeq{{{execute full trace{16381
{{ejc{{{{{16382
*      retrn (continued)
*      here to test for ftrace
{rtn02{bze{3,kvftr{6,rtn05{{jump if ftrace is off{16388
{{dcv{3,kvftr{{{else decrement ftrace{16389
*      here for print trace of function return
{rtn03{jsr{6,prtsn{{{print statement number{16393
{{mov{7,xr{13,num01(xs){{load return type{16394
{{jsr{6,prtst{{{print it{16395
{{mov{8,wa{18,=ch_bl{{load blank{16396
{{jsr{6,prtch{{{print it{16397
{{mov{7,xl{12,0(xs){{load pfblk ptr{16398
{{mov{7,xl{13,pfvbl(xl){{load function vrblk ptr{16399
{{mov{8,wa{19,*vrval{{set vrblk name offset{16400
{{bne{7,xr{21,=scfrt{6,rtn04{jump if not freturn case{16401
*      for freturn, just print function name
{{jsr{6,prtnm{{{print name{16405
{{jsr{6,prtnl{{{terminate print line{16406
{{brn{6,rtn05{{{merge{16407
*      here for return or nreturn, print function name = value
{rtn04{jsr{6,prtnv{{{print name = value{16411
*      here after completing trace
{rtn05{mov{7,xr{10,(xs)+{{pop pfblk pointer{16415
{{mov{8,wa{10,(xs)+{{pop return type string{16416
*      merge here if no trace required
{rtn06{mov{3,kvrtn{8,wa{{set rtntype keyword{16420
{{mov{7,xl{13,pfvbl(xr){{load pointer to fn vrblk{16421
{{ejc{{{{{16422
*      retrn (continued)
*      get value of function
{rtn07{mov{3,rtnbp{7,xl{{save block pointer{16427
{{mov{7,xl{13,vrval(xl){{load value{16428
{{beq{9,(xl){22,=b_trt{6,rtn07{loop back if trapped{16429
{{mov{3,rtnfv{7,xl{{else save function result value{16430
{{mov{3,rtnsv{10,(xs)+{{save original function value{16431
{{mov{7,xl{10,(xs)+{{pop saved pointer{16435
{{bze{7,xl{6,rtn7c{{no action if none{16436
{{bze{3,kvpfl{6,rtn7c{{jump if no profiling{16437
{{jsr{6,prflu{{{else profile last func stmt{16438
{{beq{3,kvpfl{18,=num02{6,rtn7a{branch on value of profile keywd{16439
*      here if &profile = 1. start time must be frigged to
*      appear earlier than it actually is, by amount used before
*      the call.
{{ldi{3,pfstm{{{load current time{16445
{{sbi{13,icval(xl){{{frig by subtracting saved amount{16446
{{brn{6,rtn7b{{{and merge{16447
*      here if &profile = 2
{rtn7a{ldi{13,icval(xl){{{load saved time{16451
*      both profile types merge here
{rtn7b{sti{3,pfstm{{{store back correct start time{16455
*      merge here if no profiling
{rtn7c{mov{8,wb{13,fargs(xr){{get number of args{16459
{{add{8,wb{13,pfnlo(xr){{add number of locals{16461
{{bze{8,wb{6,rtn10{{jump if no args/locals{16462
{{lct{8,wb{8,wb{{else set loop counter{16463
{{add{7,xr{13,pflen(xr){{and point to end of pfblk{16464
*      loop to restore functions and locals
{rtn08{mov{7,xl{11,-(xr){{load next vrblk pointer{16468
*      loop to find value block
{rtn09{mov{8,wa{7,xl{{save block pointer{16472
{{mov{7,xl{13,vrval(xl){{load pointer to next value{16473
{{beq{9,(xl){22,=b_trt{6,rtn09{loop back if trapped{16474
{{mov{7,xl{8,wa{{else restore last block pointer{16475
{{mov{13,vrval(xl){10,(xs)+{{restore old variable value{16476
{{bct{8,wb{6,rtn08{{loop till all processed{16477
*      now restore function value and exit
{rtn10{mov{7,xl{3,rtnbp{{restore ptr to last function block{16481
{{mov{13,vrval(xl){3,rtnsv{{restore old function value{16482
{{mov{7,xr{3,rtnfv{{reload function result{16483
{{mov{7,xl{3,r_cod{{point to new code block{16484
{{mov{3,kvlst{3,kvstn{{set lastno from stno{16485
{{mov{3,kvstn{13,cdstm(xl){{reset proper stno value{16486
{{mov{3,kvlln{3,kvlin{{set lastline from line{16488
{{mov{3,kvlin{13,cdsln(xl){{reset proper line value{16489
{{mov{8,wa{3,kvrtn{{load return type{16491
{{beq{8,wa{21,=scrtn{6,exixr{exit with result in xr if return{16492
{{beq{8,wa{21,=scfrt{6,exfal{fail if freturn{16493
{{ejc{{{{{16494
*      retrn (continued)
*      here for nreturn
{{beq{9,(xr){22,=b_nml{6,rtn11{jump if is a name{16500
{{jsr{6,gtnvr{{{else try convert to variable name{16501
{{err{1,243{26,function result in nreturn is not name{{{16502
{{mov{7,xl{7,xr{{if ok, copy vrblk (name base) ptr{16503
{{mov{8,wa{19,*vrval{{set name offset{16504
{{brn{6,rtn12{{{and merge{16505
*      here if returned result is a name
{rtn11{mov{7,xl{13,nmbas(xr){{load name base{16509
{{mov{8,wa{13,nmofs(xr){{load name offset{16510
*      merge here with returned name in (xl,wa)
{rtn12{mov{7,xr{7,xl{{preserve xl{16514
{{lcw{8,wb{{{load next word{16515
{{mov{7,xl{7,xr{{restore xl{16516
{{beq{8,wb{21,=ofne_{6,exnam{exit if called by name{16517
{{mov{11,-(xs){8,wb{{else save code word{16518
{{jsr{6,acess{{{get value{16519
{{ppm{6,exfal{{{fail if access fails{16520
{{mov{7,xl{7,xr{{if ok, copy result{16521
{{mov{7,xr{9,(xs){{reload next code word{16522
{{mov{9,(xs){7,xl{{store result on stack{16523
{{mov{7,xl{9,(xr){{load routine address{16524
{{bri{7,xl{{{jump to execute next code word{16525
{{ejc{{{{{16526
*      stcov -- signal statement counter overflow
*      brn  stcov            jump to signal statement count oflo
*      permit up to 10 more statements to be obeyed so that
*      setexit trap can regain control.
*      stcov continues by issuing the error message
{stcov{rtn{{{{{16536
{{icv{3,errft{{{fatal error{16537
{{ldi{4,intvt{{{get 10{16538
{{adi{3,kvstl{{{add to former limit{16539
{{sti{3,kvstl{{{store as new stlimit{16540
{{ldi{4,intvt{{{get 10{16541
{{sti{3,kvstc{{{set as new count{16542
{{jsr{6,stgcc{{{recompute countdown counters{16543
{{erb{1,244{26,statement count exceeds value of stlimit keyword{{{16544
{{ejc{{{{{16545
*      stmgo -- start execution of new statement
*      (xr)                  pointer to cdblk for new statement
*      brn  stmgo            jump to execute new statement
*      stmgo continues by executing the next statement
{stmgo{rtn{{{{{16554
{{mov{3,r_cod{7,xr{{set new code block pointer{16555
{{dcv{3,stmct{{{see if time to check something{16556
{{bze{3,stmct{6,stgo2{{jump if so{16557
{{mov{3,kvlst{3,kvstn{{set lastno{16558
{{mov{3,kvstn{13,cdstm(xr){{set stno{16559
{{mov{3,kvlln{3,kvlin{{set lastline{16561
{{mov{3,kvlin{13,cdsln(xr){{set line{16562
{{add{7,xr{19,*cdcod{{point to first code word{16564
{{lcp{7,xr{{{set code pointer{16565
*      here to execute first code word of statement
{stgo1{lcw{7,xr{{{load next code word{16569
{{zer{7,xl{{{clear garbage xl{16570
{{bri{9,(xr){{{execute it{16571
*      check profiling, polling, stlimit, statement tracing
{stgo2{bze{3,kvpfl{6,stgo3{{skip if no profiling{16575
{{jsr{6,prflu{{{else profile the statement in kvstn{16576
*      here when finished with profiling
{stgo3{mov{3,kvlst{3,kvstn{{set lastno{16580
{{mov{3,kvstn{13,cdstm(xr){{set stno{16581
{{mov{3,kvlln{3,kvlin{{set lastline{16583
{{mov{3,kvlin{13,cdsln(xr){{set line{16584
{{add{7,xr{19,*cdcod{{point to first code word{16586
{{lcp{7,xr{{{set code pointer{16587
*      here to check for polling
{{mov{11,-(xs){3,stmcs{{save present count start on stack{16592
{{dcv{3,polct{{{poll interval within stmct{16593
{{bnz{3,polct{6,stgo4{{jump if not poll time yet{16594
{{zer{8,wa{{{=0 for poll{16595
{{mov{8,wb{3,kvstn{{statement number{16596
{{mov{7,xl{7,xr{{make collectable{16597
{{jsr{6,syspl{{{allow interactive access{16598
{{err{1,320{26,user interrupt{{{16599
{{ppm{{{{single step{16600
{{ppm{{{{expression evaluation{16601
{{mov{7,xr{7,xl{{restore code block pointer{16602
{{mov{3,polcs{8,wa{{poll interval start value{16603
{{jsr{6,stgcc{{{recompute counter values{16604
*      check statement limit
{stgo4{ldi{3,kvstc{{{get stmt count{16609
{{ilt{6,stgo5{{{omit counting if negative{16610
{{mti{10,(xs)+{{{reload start value of counter{16611
{{ngi{{{{negate{16612
{{adi{3,kvstc{{{stmt count minus counter{16613
{{sti{3,kvstc{{{replace it{16614
{{ile{6,stcov{{{fail if stlimit reached{16615
{{bze{3,r_stc{6,stgo5{{jump if no statement trace{16616
{{zer{7,xr{{{clear garbage value in xr{16617
{{mov{7,xl{3,r_stc{{load pointer to stcount trblk{16618
{{jsr{6,ktrex{{{execute keyword trace{16619
*      reset stmgo counter
{stgo5{mov{3,stmct{3,stmcs{{reset counter{16623
{{brn{6,stgo1{{{fetch next code word{16624
{{ejc{{{{{16625
*      stopr -- terminate run
*      (xr)                  points to ending message
*      brn stopr             jump to terminate run
*      terminate run and print statistics.  on entry xr points
*      to ending message or is zero if message  printed already.
{stopr{rtn{{{{{16635
{{bze{7,xr{6,stpra{{skip if sysax already called{16637
{{jsr{6,sysax{{{call after execution proc{16638
{stpra{add{3,dname{3,rsmem{{use the reserve memory{16639
{{bne{7,xr{21,=endms{6,stpr0{skip if not normal end message{16643
{{bnz{3,exsts{6,stpr3{{skip if exec stats suppressed{16644
{{zer{3,erich{{{clear errors to int.ch. flag{16645
*      look to see if an ending message is supplied
{stpr0{jsr{6,prtpg{{{eject printer{16649
{{bze{7,xr{6,stpr1{{skip if no message{16650
{{jsr{6,prtst{{{print message{16651
*      merge here if no message to print
{stpr1{jsr{6,prtis{{{print blank line{16655
{{bnz{3,gbcfl{6,stpr5{{if in garbage collection, skip{16657
{{mov{7,xr{21,=stpm6{{point to message /in file xxx/{16658
{{jsr{6,prtst{{{print it{16659
{{mov{3,profs{18,=prtmf{{set column offset{16660
{{mov{8,wc{3,kvstn{{get statement number{16661
{{jsr{6,filnm{{{get file name{16662
{{mov{7,xr{7,xl{{prepare to print{16663
{{jsr{6,prtst{{{print file name{16664
{{jsr{6,prtis{{{print to interactive channel{16665
{{mov{7,xr{3,r_cod{{get code pointer{16672
{{mti{13,cdsln(xr){{{get source line number{16673
{{mov{7,xr{21,=stpm4{{point to message /in line xxx/{16674
{{jsr{6,prtmx{{{print it{16675
{stpr5{mti{3,kvstn{{{get statement number{16677
{{mov{7,xr{21,=stpm1{{point to message /in statement xxx/{16678
{{jsr{6,prtmx{{{print it{16679
{{ldi{3,kvstl{{{get statement limit{16680
{{ilt{6,stpr2{{{skip if negative{16681
{{sbi{3,kvstc{{{minus counter = course count{16682
{{sti{3,stpsi{{{save{16683
{{mov{8,wa{3,stmcs{{refine with counter start value{16684
{{sub{8,wa{3,stmct{{minus current counter{16685
{{mti{8,wa{{{convert to integer{16686
{{adi{3,stpsi{{{add in course count{16687
{{sti{3,stpsi{{{save{16688
{{mov{7,xr{21,=stpm2{{point to message /stmts executed/{16689
{{jsr{6,prtmx{{{print it{16690
{{jsr{6,systm{{{get current time{16691
{{sbi{3,timsx{{{minus start time = elapsed exec tim in nanosec{16692
{{sti{3,stpti{{{save for later{16693
{{dvi{4,intth{{{divide by 1000 to convert to microseconds{16694
{{iov{6,stpr2{{{jump if we cannot compute{16695
{{dvi{4,intth{{{divide by 1000 to convert to milliseconds{16696
{{iov{6,stpr2{{{jump if we cannot compute{16697
{{sti{3,stpti{{{save elapsed time in milliseconds{16698
{{mov{7,xr{21,=stpm3{{point to msg /execution time msec /{16699
{{jsr{6,prtmx{{{print it{16700
*      Only list peformance statistics giving stmts / millisec, etc.
*      if program ran for more than one millisecond.
{{ldi{3,stpti{{{reload execution time in milliseconds{16705
{{ile{6,stpr2{{{jump if exection time less than a millisecond{16706
{{ldi{3,stpsi{{{load statement count{16710
{{dvi{3,stpti{{{divide to get stmts per millisecond{16711
{{iov{6,stpr2{{{jump if we cannot compute{16712
{{dvi{4,intth{{{divide to get stmts per microsecond{16713
{{iov{6,stpr2{{{jump if we cannot compute{16714
{{mov{7,xr{21,=stpm7{{point to msg (stmt / microsec){16715
{{jsr{6,prtmx{{{print it{16716
{{ldi{3,stpsi{{{reload statement count{16718
{{dvi{3,stpti{{{divide to get stmts per millisecond{16719
{{iov{6,stpr2{{{jump if we cannot compute{16720
{{mov{7,xr{21,=stpm8{{point to msg (stmt / millisec ){16721
{{jsr{6,prtmx{{{print it{16722
{{ldi{3,stpsi{{{reload statement count{16724
{{dvi{3,stpti{{{divide to get stmts per millisecond{16725
{{iov{6,stpr2{{{jump if we cannot compute{16726
{{mli{4,intth{{{multiply by 1000 to get stmts per microsecond{16727
{{iov{6,stpr2{{{jump if overflow{16728
{{mov{7,xr{21,=stpm9{{point to msg ( stmt / second ){16729
{{jsr{6,prtmx{{{print it{16730
{{ejc{{{{{16732
*      stopr (continued)
*      merge to skip message (overflow or negative stlimit)
{stpr2{mti{3,gbcnt{{{load count of collections{16738
{{mov{7,xr{21,=stpm4{{point to message /regenerations /{16739
{{jsr{6,prtmx{{{print it{16740
{{jsr{6,prtmm{{{print memory usage{16741
{{jsr{6,prtis{{{one more blank for luck{16742
*      check if dump requested
{stpr3{jsr{6,prflr{{{print profile if wanted{16749
{{mov{7,xr{3,kvdmp{{load dump keyword{16751
{{jsr{6,dumpr{{{execute dump if requested{16753
{{mov{7,xl{3,r_fcb{{get fcblk chain head{16754
{{mov{8,wa{3,kvabe{{load abend value{16755
{{mov{8,wb{3,kvcod{{load code value{16756
{{jsr{6,sysej{{{exit to system{16757
*      here after sysea call and suppressing error msg print
{stpr4{rtn{{{{{16762
{{add{3,dname{3,rsmem{{use the reserve memory{16763
{{bze{3,exsts{6,stpr1{{if execution stats requested{16764
{{brn{6,stpr3{{{check if dump or profile needed{16765
{{ejc{{{{{16768
*      succp -- signal successful match of a pattern node
*      see pattern match routines for details
*      (xr)                  current node
*      (wb)                  current cursor
*      (xl)                  may be non-collectable
*      brn  succp            signal successful pattern match
*      succp continues by matching the successor node
{succp{rtn{{{{{16781
{{mov{7,xr{13,pthen(xr){{load successor node{16782
{{mov{7,xl{9,(xr){{load node code entry address{16783
{{bri{7,xl{{{jump to match successor node{16784
{{ejc{{{{{16785
*      sysab -- print /abnormal end/ and terminate
{sysab{rtn{{{{{16789
{{mov{7,xr{21,=endab{{point to message{16790
{{mov{3,kvabe{18,=num01{{set abend flag{16791
{{jsr{6,prtnl{{{skip to new line{16792
{{brn{6,stopr{{{jump to pack up{16793
{{ejc{{{{{16794
*      systu -- print /time up/ and terminate
{systu{rtn{{{{{16798
{{mov{7,xr{21,=endtu{{point to message{16799
{{mov{8,wa{4,strtu{{get chars /tu/{16800
{{mov{3,kvcod{8,wa{{put in kvcod{16801
{{mov{8,wa{3,timup{{check state of timeup switch{16802
{{mnz{3,timup{{{set switch{16803
{{bnz{8,wa{6,stopr{{stop run if already set{16804
{{erb{1,245{26,translation/execution time expired{{{16805
{{ttl{27,s p i t b o l -- utility procedures{{{{16806
*      the following section contains procedures which are
*      used for various purposes throughout the system.
*      each procedure is preceded by a description of the
*      calling sequence. usually the arguments are in registers
*      but arguments can also occur on the stack and as
*      parameters assembled after the jsr instruction.
*      the following considerations apply to these descriptions.
*      1)   the stack pointer (xs) is not changed unless the
*           change is explicitly documented in the call.
*      2)   registers whose entry values are not mentioned
*           may contain any value except that xl,xr may only
*           contain proper (collectable) pointer values.
*           this condition on means that the called routine
*           may if it chooses preserve xl,xr by stacking.
*      3)   registers not mentioned on exit contain the same
*           values as they did on entry except that values in
*           xr,xl may have been relocated by the collector.
*      4)   registers which are destroyed on exit may contain
*           any value except that values in xl,xr are proper
*           (collectable) pointers.
*      5)   the code pointer register points to the current
*           code location on entry and is unchanged on exit.
*      in the above description, a collectable pointer is one
*      which either points outside the dynamic region or
*      points to the start of a block in the dynamic region.
*      in those cases where the calling sequence contains
*      parameters which are used as alternate return points,
*      these parameters may be replaced by error codes
*      assembled with the err instruction. this will result
*      in the posting of the error if the return is taken.
*      the procedures all have names consisting of five letters
*      and are in alphabetical order by their names.
{{ejc{{{{{16850
*      acess - access variable value with trace/input checks
*      acess loads the value of a variable. trace and input
*      associations are tested for and executed as required.
*      acess also handles the special cases of pseudo-variables.
*      (xl)                  variable name base
*      (wa)                  variable name offset
*      jsr  acess            call to access value
*      ppm  loc              transfer loc if access failure
*      (xr)                  variable value
*      (wa,wb,wc)            destroyed
*      (xl,ra)               destroyed
*      failure can occur if an input association causes an end
*      of file condition or if the evaluation of an expression
*      associated with an expression variable fails.
{acess{prc{25,r{1,1{{entry point (recursive){16870
{{mov{7,xr{7,xl{{copy name base{16871
{{add{7,xr{8,wa{{point to variable location{16872
{{mov{7,xr{9,(xr){{load variable value{16873
*      loop here to check for successive trblks
{acs02{bne{9,(xr){22,=b_trt{6,acs18{jump if not trapped{16877
*      here if trapped
{{beq{7,xr{21,=trbkv{6,acs12{jump if keyword variable{16881
{{bne{7,xr{21,=trbev{6,acs05{jump if not expression variable{16882
*      here for expression variable, evaluate variable
{{mov{7,xr{13,evexp(xl){{load expression pointer{16886
{{zer{8,wb{{{evaluate by value{16887
{{jsr{6,evalx{{{evaluate expression{16888
{{ppm{6,acs04{{{jump if evaluation failure{16889
{{brn{6,acs02{{{check value for more trblks{16890
{{ejc{{{{{16891
*      acess (continued)
*      here on reading end of file
{acs03{add{7,xs{19,*num03{{pop trblk ptr, name base and offset{16897
{{mov{3,dnamp{7,xr{{pop unused scblk{16898
*      merge here when evaluation of expression fails
{acs04{exi{1,1{{{take alternate (failure) return{16902
*      here if not keyword or expression variable
{acs05{mov{8,wb{13,trtyp(xr){{load trap type code{16906
{{bnz{8,wb{6,acs10{{jump if not input association{16907
{{bze{3,kvinp{6,acs09{{ignore input assoc if input is off{16908
*      here for input association
{{mov{11,-(xs){7,xl{{stack name base{16912
{{mov{11,-(xs){8,wa{{stack name offset{16913
{{mov{11,-(xs){7,xr{{stack trblk pointer{16914
{{mov{3,actrm{3,kvtrm{{temp to hold trim keyword{16915
{{mov{7,xl{13,trfpt(xr){{get file ctrl blk ptr or zero{16916
{{bnz{7,xl{6,acs06{{jump if not standard input file{16917
{{beq{13,trter(xr){21,=v_ter{6,acs21{jump if terminal{16918
*      here to read from standard input file
{{mov{8,wa{3,cswin{{length for read buffer{16922
{{jsr{6,alocs{{{build string of appropriate length{16923
{{jsr{6,sysrd{{{read next standard input image{16924
{{ppm{6,acs03{{{jump to fail exit if end of file{16925
{{brn{6,acs07{{{else merge with other file case{16926
*      here for input from other than standard input file
{acs06{mov{8,wa{7,xl{{fcblk ptr{16930
{{jsr{6,sysil{{{get input record max length (to wa){16931
{{bnz{8,wc{6,acs6a{{jump if not binary file{16932
{{mov{3,actrm{8,wc{{disable trim for binary file{16933
{acs6a{jsr{6,alocs{{{allocate string of correct size{16934
{{mov{8,wa{7,xl{{fcblk ptr{16935
{{jsr{6,sysin{{{call system input routine{16936
{{ppm{6,acs03{{{jump to fail exit if end of file{16937
{{ppm{6,acs22{{{error{16938
{{ppm{6,acs23{{{error{16939
{{ejc{{{{{16940
*      acess (continued)
*      merge here after obtaining input record
{acs07{mov{8,wb{3,actrm{{load trim indicator{16946
{{jsr{6,trimr{{{trim record as required{16947
{{mov{8,wb{7,xr{{copy result pointer{16948
{{mov{7,xr{9,(xs){{reload pointer to trblk{16949
*      loop to chase to end of trblk chain and store value
{acs08{mov{7,xl{7,xr{{save pointer to this trblk{16953
{{mov{7,xr{13,trnxt(xr){{load forward pointer{16954
{{beq{9,(xr){22,=b_trt{6,acs08{loop if this is another trblk{16955
{{mov{13,trnxt(xl){8,wb{{else store result at end of chain{16956
{{mov{7,xr{10,(xs)+{{restore initial trblk pointer{16957
{{mov{8,wa{10,(xs)+{{restore name offset{16958
{{mov{7,xl{10,(xs)+{{restore name base pointer{16959
*      come here to move to next trblk
{acs09{mov{7,xr{13,trnxt(xr){{load forward ptr to next value{16963
{{brn{6,acs02{{{back to check if trapped{16964
*      here to check for access trace trblk
{acs10{bne{8,wb{18,=trtac{6,acs09{loop back if not access trace{16968
{{bze{3,kvtra{6,acs09{{ignore access trace if trace off{16969
{{dcv{3,kvtra{{{else decrement trace count{16970
{{bze{13,trfnc(xr){6,acs11{{jump if print trace{16971
{{ejc{{{{{16972
*      acess (continued)
*      here for full function trace
{{jsr{6,trxeq{{{call routine to execute trace{16978
{{brn{6,acs09{{{jump for next trblk{16979
*      here for case of print trace
{acs11{jsr{6,prtsn{{{print statement number{16983
{{jsr{6,prtnv{{{print name = value{16984
{{brn{6,acs09{{{jump back for next trblk{16985
*      here for keyword variable
{acs12{mov{7,xr{13,kvnum(xl){{load keyword number{16989
{{bge{7,xr{18,=k_v__{6,acs14{jump if not one word value{16990
{{mti{15,kvabe(xr){{{else load value as integer{16991
*      common exit with keyword value as integer in (ia)
{acs13{jsr{6,icbld{{{build icblk{16995
{{brn{6,acs18{{{jump to exit{16996
*      here if not one word keyword value
{acs14{bge{7,xr{18,=k_s__{6,acs15{jump if special case{17000
{{sub{7,xr{18,=k_v__{{else get offset{17001
{{wtb{7,xr{{{convert to byte offset{17002
{{add{7,xr{21,=ndabo{{point to pattern value{17003
{{brn{6,acs18{{{jump to exit{17004
*      here if special keyword case
{acs15{mov{7,xl{3,kvrtn{{load rtntype in case{17008
{{ldi{3,kvstl{{{load stlimit in case{17009
{{sub{7,xr{18,=k_s__{{get case number{17010
{{bsw{7,xr{2,k__n_{{switch on keyword number{17011
{{iff{2,k__al{6,acs16{{jump if alphabet{17025
{{iff{2,k__rt{6,acs17{{rtntype{17025
{{iff{2,k__sc{6,acs19{{stcount{17025
{{iff{2,k__et{6,acs20{{errtext{17025
{{iff{2,k__fl{6,acs26{{file{17025
{{iff{2,k__lf{6,acs27{{lastfile{17025
{{iff{2,k__sl{6,acs13{{stlimit{17025
{{iff{2,k__lc{6,acs24{{lcase{17025
{{iff{2,k__uc{6,acs25{{ucase{17025
{{esw{{{{end switch on keyword number{17025
{{ejc{{{{{17026
*      acess (continued)
*      lcase
{acs24{mov{7,xr{21,=lcase{{load pointer to lcase string{17033
{{brn{6,acs18{{{common return{17034
*      ucase
{acs25{mov{7,xr{21,=ucase{{load pointer to ucase string{17038
{{brn{6,acs18{{{common return{17039
*      file
{acs26{mov{8,wc{3,kvstn{{load current stmt number{17045
{{brn{6,acs28{{{merge to obtain file name{17046
*      lastfile
{acs27{mov{8,wc{3,kvlst{{load last stmt number{17050
*      merge here to map statement number in wc to file name
{acs28{jsr{6,filnm{{{obtain file name for this stmt{17054
{{brn{6,acs17{{{merge to return string in xl{17055
*      alphabet
{acs16{mov{7,xl{3,kvalp{{load pointer to alphabet string{17059
*      rtntype merges here
{acs17{mov{7,xr{7,xl{{copy string ptr to proper reg{17063
*      common return point
{acs18{exi{{{{return to acess caller{17067
*      here for stcount (ia has stlimit)
{acs19{ilt{6,acs29{{{if counting suppressed{17071
{{mov{8,wa{3,stmcs{{refine with counter start value{17072
{{sub{8,wa{3,stmct{{minus current counter{17073
{{mti{8,wa{{{convert to integer{17074
{{adi{3,kvstl{{{add stlimit{17075
{acs29{sbi{3,kvstc{{{stcount = limit - left{17076
{{brn{6,acs13{{{merge back with integer result{17077
*      errtext
{acs20{mov{7,xr{3,r_etx{{get errtext string{17081
{{brn{6,acs18{{{merge with result{17082
*      here to read a record from terminal
{acs21{mov{8,wa{18,=rilen{{buffer length{17086
{{jsr{6,alocs{{{allocate buffer{17087
{{jsr{6,sysri{{{read record{17088
{{ppm{6,acs03{{{endfile{17089
{{brn{6,acs07{{{merge with record read{17090
*      error returns
{acs22{mov{3,dnamp{7,xr{{pop unused scblk{17094
{{erb{1,202{26,input from file caused non-recoverable error{{{17095
{acs23{mov{3,dnamp{7,xr{{pop unused scblk{17097
{{erb{1,203{26,input file record has incorrect format{{{17098
{{enp{{{{end procedure acess{17099
{{ejc{{{{{17100
*      acomp -- compare two arithmetic values
*      1(xs)                 first argument
*      0(xs)                 second argument
*      jsr  acomp            call to compare values
*      ppm  loc              transfer loc if arg1 is non-numeric
*      ppm  loc              transfer loc if arg2 is non-numeric
*      ppm  loc              transfer loc for arg1 lt arg2
*      ppm  loc              transfer loc for arg1 eq arg2
*      ppm  loc              transfer loc for arg1 gt arg2
*      (normal return is never given)
*      (wa,wb,wc,ia,ra)      destroyed
*      (xl,xr)               destroyed
{acomp{prc{25,n{1,5{{entry point{17116
{{jsr{6,arith{{{load arithmetic operands{17117
{{ppm{6,acmp7{{{jump if first arg non-numeric{17118
{{ppm{6,acmp8{{{jump if second arg non-numeric{17119
{{ppm{6,acmp4{{{jump if real arguments{17122
*      here for integer arguments
{{sbi{13,icval(xl){{{subtract to compare{17127
{{iov{6,acmp3{{{jump if overflow{17128
{{ilt{6,acmp5{{{else jump if arg1 lt arg2{17129
{{ieq{6,acmp2{{{jump if arg1 eq arg2{17130
*      here if arg1 gt arg2
{acmp1{exi{1,5{{{take gt exit{17134
*      here if arg1 eq arg2
{acmp2{exi{1,4{{{take eq exit{17138
{{ejc{{{{{17139
*      acomp (continued)
*      here for integer overflow on subtract
{acmp3{ldi{13,icval(xl){{{load second argument{17145
{{ilt{6,acmp1{{{gt if negative{17146
{{brn{6,acmp5{{{else lt{17147
*      here for real operands
{acmp4{sbr{13,rcval(xl){{{subtract to compare{17153
{{rov{6,acmp6{{{jump if overflow{17154
{{rgt{6,acmp1{{{else jump if arg1 gt{17155
{{req{6,acmp2{{{jump if arg1 eq arg2{17156
*      here if arg1 lt arg2
{acmp5{exi{1,3{{{take lt exit{17161
*      here if overflow on real subtraction
{acmp6{ldr{13,rcval(xl){{{reload arg2{17167
{{rlt{6,acmp1{{{gt if negative{17168
{{brn{6,acmp5{{{else lt{17169
*      here if arg1 non-numeric
{acmp7{exi{1,1{{{take error exit{17174
*      here if arg2 non-numeric
{acmp8{exi{1,2{{{take error exit{17178
{{enp{{{{end procedure acomp{17179
{{ejc{{{{{17180
*      alloc                 allocate block of dynamic storage
*      (wa)                  length required in bytes
*      jsr  alloc            call to allocate block
*      (xr)                  pointer to allocated block
*      a possible alternative to aov ... and following stmt is -
*      mov  dname,xr .  sub  wa,xr .  blo xr,dnamp,aloc2 .
*      mov  dnamp,xr .  add  wa,xr
{alloc{prc{25,e{1,0{{entry point{17192
*      common exit point
{aloc1{mov{7,xr{3,dnamp{{point to next available loc{17196
{{aov{8,wa{7,xr{6,aloc2{point past allocated block{17197
{{bgt{7,xr{3,dname{6,aloc2{jump if not enough room{17198
{{mov{3,dnamp{7,xr{{store new pointer{17199
{{sub{7,xr{8,wa{{point back to start of allocated bk{17200
{{exi{{{{return to caller{17201
*      here if insufficient room, try a garbage collection
{aloc2{mov{3,allsv{8,wb{{save wb{17205
{alc2a{zer{8,wb{{{set no upward move for gbcol{17206
{{jsr{6,gbcol{{{garbage collect{17207
{{mov{8,wb{7,xr{{remember new sediment size{17209
*      see if room after gbcol or sysmm call
{aloc3{mov{7,xr{3,dnamp{{point to first available loc{17214
{{aov{8,wa{7,xr{6,alc3a{point past new block{17215
{{blo{7,xr{3,dname{6,aloc4{jump if there is room now{17216
*      failed again, see if we can get more core
{alc3a{jsr{6,sysmm{{{try to get more memory{17220
{{wtb{7,xr{{{convert to baus (sgd05){17221
{{add{3,dname{7,xr{{bump ptr by amount obtained{17222
{{bnz{7,xr{6,aloc3{{jump if got more core{17223
{{bze{3,dnams{6,alc3b{{jump if there was no sediment{17225
{{zer{3,dnams{{{try collecting the sediment{17226
{{brn{6,alc2a{{{{17227
*      sysmm failed and there was no sediment to collect
{alc3b{add{3,dname{3,rsmem{{get the reserve memory{17231
{{zer{3,rsmem{{{only permissible once{17235
{{icv{3,errft{{{fatal error{17236
{{erb{1,204{26,memory overflow{{{17237
{{ejc{{{{{17238
*      here after successful garbage collection
{aloc4{sti{3,allia{{{save ia{17242
{{mov{3,dnams{8,wb{{record new sediment size{17244
{{mov{8,wb{3,dname{{get dynamic end adrs{17246
{{sub{8,wb{3,dnamp{{compute free store{17247
{{btw{8,wb{{{convert bytes to words{17248
{{mti{8,wb{{{put free store in ia{17249
{{mli{3,alfsf{{{multiply by free store factor{17250
{{iov{6,aloc5{{{jump if overflowed{17251
{{mov{8,wb{3,dname{{dynamic end adrs{17252
{{sub{8,wb{3,dnamb{{compute total amount of dynamic{17253
{{btw{8,wb{{{convert to words{17254
{{mov{3,aldyn{8,wb{{store it{17255
{{sbi{3,aldyn{{{subtract from scaled up free store{17256
{{igt{6,aloc5{{{jump if sufficient free store{17257
{{jsr{6,sysmm{{{try to get more store{17258
{{wtb{7,xr{{{convert to baus (sgd05){17259
{{add{3,dname{7,xr{{adjust dynamic end adrs{17260
*      merge to restore ia and wb
{aloc5{ldi{3,allia{{{recover ia{17264
{{mov{8,wb{3,allsv{{restore wb{17265
{{brn{6,aloc1{{{jump back to exit{17266
{{enp{{{{end procedure alloc{17267
{{ejc{{{{{17268
*      alocs -- allocate string block
*      alocs is used to build a frame for a string block into
*      which the actual characters are placed by the caller.
*      all strings are created with a call to alocs (the
*      exceptions occur in trimr and s_rpl procedures).
*      (wa)                  length of string to be allocated
*      jsr  alocs            call to allocate scblk
*      (xr)                  pointer to resulting scblk
*      (wa)                  destroyed
*      (wc)                  character count (entry value of wa)
*      the resulting scblk has the type word and the length
*      filled in and the last word is cleared to zero characters
*      to ensure correct right padding of the final word.
{alocs{prc{25,e{1,0{{entry point{17328
{{bgt{8,wa{3,kvmxl{6,alcs2{jump if length exceeds maxlength{17329
{{mov{8,wc{8,wa{{else copy length{17330
{{ctb{8,wa{2,scsi_{{compute length of scblk in bytes{17331
{{mov{7,xr{3,dnamp{{point to next available location{17332
{{aov{8,wa{7,xr{6,alcs0{point past block{17333
{{blo{7,xr{3,dname{6,alcs1{jump if there is room{17334
*      insufficient memory
{alcs0{zer{7,xr{{{else clear garbage xr value{17338
{{jsr{6,alloc{{{and use standard allocator{17339
{{add{7,xr{8,wa{{point past end of block to merge{17340
*      merge here with xr pointing beyond new block
{alcs1{mov{3,dnamp{7,xr{{set updated storage pointer{17344
{{zer{11,-(xr){{{store zero chars in last word{17345
{{dca{8,wa{{{decrement length{17346
{{sub{7,xr{8,wa{{point back to start of block{17347
{{mov{9,(xr){22,=b_scl{{set type word{17348
{{mov{13,sclen(xr){8,wc{{store length in chars{17349
{{exi{{{{return to alocs caller{17350
*      come here if string is too long
{alcs2{erb{1,205{26,string length exceeds value of maxlngth keyword{{{17354
{{enp{{{{end procedure alocs{17355
{{ejc{{{{{17356
*      alost -- allocate space in static region
*      (wa)                  length required in bytes
*      jsr  alost            call to allocate space
*      (xr)                  pointer to allocated block
*      (wb)                  destroyed
*      note that the coding ensures that the resulting value
*      of state is always less than dnamb. this fact is used
*      in testing a variable name for being in the static region
{alost{prc{25,e{1,0{{entry point{17369
*      merge back here after allocating new chunk
{alst1{mov{7,xr{3,state{{point to current end of area{17373
{{aov{8,wa{7,xr{6,alst2{point beyond proposed block{17374
{{bge{7,xr{3,dnamb{6,alst2{jump if overlap with dynamic area{17375
{{mov{3,state{7,xr{{else store new pointer{17376
{{sub{7,xr{8,wa{{point back to start of block{17377
{{exi{{{{return to alost caller{17378
*      here if no room, prepare to move dynamic storage up
{alst2{mov{3,alsta{8,wa{{save wa{17382
{{bge{8,wa{19,*e_sts{6,alst3{skip if requested chunk is large{17383
{{mov{8,wa{19,*e_sts{{else set to get large enough chunk{17384
*      here with amount to move up in wa
{alst3{jsr{6,alloc{{{allocate block to ensure room{17388
{{mov{3,dnamp{7,xr{{and delete it{17389
{{mov{8,wb{8,wa{{copy move up amount{17390
{{jsr{6,gbcol{{{call gbcol to move dynamic area up{17391
{{mov{3,dnams{7,xr{{remember new sediment size{17393
{{mov{8,wa{3,alsta{{restore wa{17395
{{brn{6,alst1{{{loop back to try again{17396
{{enp{{{{end procedure alost{17397
{{ejc{{{{{17398
*      arith -- fetch arithmetic operands
*      arith is used by functions and operators which expect
*      two numeric arguments (operands) which must both be
*      integer or both be real. arith fetches two arguments from
*      the stack and performs any necessary conversions.
*      1(xs)                 first argument (left operand)
*      0(xs)                 second argument (right operand)
*      jsr  arith            call to fetch numeric arguments
*      ppm  loc              transfer loc for opnd 1 non-numeric
*      ppm  loc              transfer loc for opnd 2 non-numeric
*      ppm  loc              transfer loc for real operands
*      for integer args, control returns past the parameters
*      (ia)                  left operand value
*      (xr)                  ptr to icblk for left operand
*      (xl)                  ptr to icblk for right operand
*      (xs)                  popped twice
*      (wa,wb,ra)            destroyed
*      for real arguments, control returns to the location
*      specified by the third parameter.
*      (ra)                  left operand value
*      (xr)                  ptr to rcblk for left operand
*      (xl)                  ptr to rcblk for right operand
*      (wa,wb,wc)            destroyed
*      (xs)                  popped twice
{{ejc{{{{{17472
*      arith (continued)
*      entry point
{arith{prc{25,n{1,3{{entry point{17481
{{mov{7,xl{10,(xs)+{{load right operand{17483
{{mov{7,xr{10,(xs)+{{load left operand{17484
{{mov{8,wa{9,(xl){{get right operand type word{17485
{{beq{8,wa{22,=b_icl{6,arth1{jump if integer{17486
{{beq{8,wa{22,=b_rcl{6,arth4{jump if real{17489
{{mov{11,-(xs){7,xr{{else replace left arg on stack{17491
{{mov{7,xr{7,xl{{copy left arg pointer{17492
{{jsr{6,gtnum{{{convert to numeric{17493
{{ppm{6,arth6{{{jump if unconvertible{17494
{{mov{7,xl{7,xr{{else copy converted result{17495
{{mov{8,wa{9,(xl){{get right operand type word{17496
{{mov{7,xr{10,(xs)+{{reload left argument{17497
{{beq{8,wa{22,=b_rcl{6,arth4{jump if right arg is real{17500
*      here if right arg is an integer
{arth1{bne{9,(xr){22,=b_icl{6,arth3{jump if left arg not integer{17505
*      exit for integer case
{arth2{ldi{13,icval(xr){{{load left operand value{17509
{{exi{{{{return to arith caller{17510
*      here for right operand integer, left operand not
{arth3{jsr{6,gtnum{{{convert left arg to numeric{17514
{{ppm{6,arth7{{{jump if not convertible{17515
{{beq{8,wa{22,=b_icl{6,arth2{jump back if integer-integer{17516
*      here we must convert real-integer to real-real
{{mov{11,-(xs){7,xr{{put left arg back on stack{17522
{{ldi{13,icval(xl){{{load right argument value{17523
{{itr{{{{convert to real{17524
{{jsr{6,rcbld{{{get real block for right arg, merge{17525
{{mov{7,xl{7,xr{{copy right arg ptr{17526
{{mov{7,xr{10,(xs)+{{load left argument{17527
{{brn{6,arth5{{{merge for real-real case{17528
{{ejc{{{{{17529
*      arith (continued)
*      here if right argument is real
{arth4{beq{9,(xr){22,=b_rcl{6,arth5{jump if left arg real{17535
{{jsr{6,gtrea{{{else convert to real{17536
{{ppm{6,arth7{{{error if unconvertible{17537
*      here for real-real
{arth5{ldr{13,rcval(xr){{{load left operand value{17541
{{exi{1,3{{{take real-real exit{17542
*      here for error converting right argument
{arth6{ica{7,xs{{{pop unwanted left arg{17547
{{exi{1,2{{{take appropriate error exit{17548
*      here for error converting left operand
{arth7{exi{1,1{{{take appropriate error return{17552
{{enp{{{{end procedure arith{17553
{{ejc{{{{{17554
*      asign -- perform assignment
*      asign performs the assignment of a value to a variable
*      with appropriate checks for output associations and
*      value trace associations which are executed as required.
*      asign also handles the special cases of assignment to
*      pattern and expression variables.
*      (wb)                  value to be assigned
*      (xl)                  base pointer for variable
*      (wa)                  offset for variable
*      jsr  asign            call to assign value to variable
*      ppm  loc              transfer loc for failure
*      (xr,xl,wa,wb,wc)      destroyed
*      (ra)                  destroyed
*      failure occurs if the evaluation of an expression
*      associated with an expression variable fails.
{asign{prc{25,r{1,1{{entry point (recursive){17575
*      merge back here to assign result to expression variable.
{asg01{add{7,xl{8,wa{{point to variable value{17579
{{mov{7,xr{9,(xl){{load variable value{17580
{{beq{9,(xr){22,=b_trt{6,asg02{jump if trapped{17581
{{mov{9,(xl){8,wb{{else perform assignment{17582
{{zer{7,xl{{{clear garbage value in xl{17583
{{exi{{{{and return to asign caller{17584
*      here if value is trapped
{asg02{sub{7,xl{8,wa{{restore name base{17588
{{beq{7,xr{21,=trbkv{6,asg14{jump if keyword variable{17589
{{bne{7,xr{21,=trbev{6,asg04{jump if not expression variable{17590
*      here for assignment to expression variable
{{mov{7,xr{13,evexp(xl){{point to expression{17594
{{mov{11,-(xs){8,wb{{store value to assign on stack{17595
{{mov{8,wb{18,=num01{{set for evaluation by name{17596
{{jsr{6,evalx{{{evaluate expression by name{17597
{{ppm{6,asg03{{{jump if evaluation fails{17598
{{mov{8,wb{10,(xs)+{{else reload value to assign{17599
{{brn{6,asg01{{{loop back to perform assignment{17600
{{ejc{{{{{17601
*      asign (continued)
*      here for failure during expression evaluation
{asg03{ica{7,xs{{{remove stacked value entry{17607
{{exi{1,1{{{take failure exit{17608
*      here if not keyword or expression variable
{asg04{mov{11,-(xs){7,xr{{save ptr to first trblk{17612
*      loop to chase down trblk chain and assign value at end
{asg05{mov{8,wc{7,xr{{save ptr to this trblk{17616
{{mov{7,xr{13,trnxt(xr){{point to next trblk{17617
{{beq{9,(xr){22,=b_trt{6,asg05{loop back if another trblk{17618
{{mov{7,xr{8,wc{{else point back to last trblk{17619
{{mov{13,trval(xr){8,wb{{store value at end of chain{17620
{{mov{7,xr{10,(xs)+{{restore ptr to first trblk{17621
*      loop to process trblk entries on chain
{asg06{mov{8,wb{13,trtyp(xr){{load type code of trblk{17625
{{beq{8,wb{18,=trtvl{6,asg08{jump if value trace{17626
{{beq{8,wb{18,=trtou{6,asg10{jump if output association{17627
*      here to move to next trblk on chain
{asg07{mov{7,xr{13,trnxt(xr){{point to next trblk on chain{17631
{{beq{9,(xr){22,=b_trt{6,asg06{loop back if another trblk{17632
{{exi{{{{else end of chain, return to caller{17633
*      here to process value trace
{asg08{bze{3,kvtra{6,asg07{{ignore value trace if trace off{17637
{{dcv{3,kvtra{{{else decrement trace count{17638
{{bze{13,trfnc(xr){6,asg09{{jump if print trace{17639
{{jsr{6,trxeq{{{else execute function trace{17640
{{brn{6,asg07{{{and loop back{17641
{{ejc{{{{{17642
*      asign (continued)
*      here for print trace
{asg09{jsr{6,prtsn{{{print statement number{17648
{{jsr{6,prtnv{{{print name = value{17649
{{brn{6,asg07{{{loop back for next trblk{17650
*      here for output association
{asg10{bze{3,kvoup{6,asg07{{ignore output assoc if output off{17654
{asg1b{mov{7,xl{7,xr{{copy trblk pointer{17655
{{mov{7,xr{13,trnxt(xr){{point to next trblk{17656
{{beq{9,(xr){22,=b_trt{6,asg1b{loop back if another trblk{17657
{{mov{7,xr{7,xl{{else point back to last trblk{17658
{{mov{11,-(xs){13,trval(xr){{stack value to output{17660
{{jsr{6,gtstg{{{convert to string{17666
{{ppm{6,asg12{{{get datatype name if unconvertible{17667
*      merge with string or buffer to output in xr
{asg11{mov{8,wa{13,trfpt(xl){{fcblk ptr{17671
{{bze{8,wa{6,asg13{{jump if standard output file{17672
*      here for output to file
{asg1a{jsr{6,sysou{{{call system output routine{17676
{{err{1,206{26,output caused file overflow{{{17677
{{err{1,207{26,output caused non-recoverable error{{{17678
{{exi{{{{else all done, return to caller{17679
*      if not printable, get datatype name instead
{asg12{jsr{6,dtype{{{call datatype routine{17683
{{brn{6,asg11{{{merge{17684
*      here to print a string to standard output or terminal
{asg13{beq{13,trter(xl){21,=v_ter{6,asg1a{jump if terminal output{17689
{{icv{8,wa{{{signal standard output{17690
{{brn{6,asg1a{{{use sysou to perform output{17691
{{ejc{{{{{17706
*      asign (continued)
*      here for keyword assignment
{asg14{mov{7,xl{13,kvnum(xl){{load keyword number{17712
{{beq{7,xl{18,=k_etx{6,asg19{jump if errtext{17713
{{mov{7,xr{8,wb{{copy value to be assigned{17714
{{jsr{6,gtint{{{convert to integer{17715
{{err{1,208{26,keyword value assigned is not integer{{{17716
{{ldi{13,icval(xr){{{else load value{17717
{{beq{7,xl{18,=k_stl{6,asg16{jump if special case of stlimit{17718
{{mfi{8,wa{6,asg18{{else get addr integer, test ovflow{17719
{{bgt{8,wa{3,mxlen{6,asg18{fail if too large{17720
{{beq{7,xl{18,=k_ert{6,asg17{jump if special case of errtype{17721
{{beq{7,xl{18,=k_pfl{6,asg21{jump if special case of profile{17724
{{beq{7,xl{18,=k_mxl{6,asg24{jump if special case of maxlngth{17726
{{beq{7,xl{18,=k_fls{6,asg26{jump if special case of fullscan{17727
{{blt{7,xl{18,=k_p__{6,asg15{jump unless protected{17728
{{erb{1,209{26,keyword in assignment is protected{{{17729
*      here to do assignment if not protected
{asg15{mov{15,kvabe(xl){8,wa{{store new value{17733
{{exi{{{{return to asign caller{17734
*      here for special case of stlimit
*      since stcount is maintained as (stlimit-stcount)
*      it is also necessary to modify stcount appropriately.
{asg16{sbi{3,kvstl{{{subtract old limit{17741
{{adi{3,kvstc{{{add old counter{17742
{{sti{3,kvstc{{{store course counter value{17743
{{ldi{3,kvstl{{{check if counting suppressed{17744
{{ilt{6,asg25{{{do not refine if so{17745
{{mov{8,wa{3,stmcs{{refine with counter breakout{17746
{{sub{8,wa{3,stmct{{values{17747
{{mti{8,wa{{{convert to integer{17748
{{ngi{{{{current-start value{17749
{{adi{3,kvstc{{{add in course counter value{17750
{{sti{3,kvstc{{{save refined value{17751
{asg25{ldi{13,icval(xr){{{reload new limit value{17752
{{sti{3,kvstl{{{store new limit value{17753
{{jsr{6,stgcc{{{recompute countdown counters{17754
{{exi{{{{return to asign caller{17755
*      here for special case of errtype
{asg17{ble{8,wa{18,=nini9{6,error{ok to signal if in range{17759
*      here if value assigned is out of range
{asg18{erb{1,210{26,keyword value assigned is negative or too large{{{17763
*      here for special case of errtext
{asg19{mov{11,-(xs){8,wb{{stack value{17767
{{jsr{6,gtstg{{{convert to string{17768
{{err{1,211{26,value assigned to keyword errtext not a string{{{17769
{{mov{3,r_etx{7,xr{{make assignment{17770
{{exi{{{{return to caller{17771
*      here for keyword profile
{asg21{bgt{8,wa{18,=num02{6,asg18{moan if not 0,1, or 2{17785
{{bze{8,wa{6,asg15{{just assign if zero{17786
{{bze{3,pfdmp{6,asg22{{branch if first assignment{17787
{{beq{8,wa{3,pfdmp{6,asg23{also if same value as before{17788
{{erb{1,268{26,inconsistent value assigned to keyword profile{{{17789
{asg22{mov{3,pfdmp{8,wa{{note value on first assignment{17791
{asg23{mov{3,kvpfl{8,wa{{store new value{17792
{{jsr{6,stgcc{{{recompute countdown counts{17793
{{jsr{6,systm{{{get the time{17794
{{sti{3,pfstm{{{fudge some kind of start time{17795
{{exi{{{{return to asign caller{17796
*      here for keyword maxlngth
{asg24{bge{8,wa{18,=mnlen{6,asg15{if acceptable value{17801
{{erb{1,287{26,value assigned to keyword maxlngth is too small{{{17802
*      here for keyword fullscan
{asg26{bnz{8,wa{6,asg15{{if acceptable value{17806
{{erb{1,274{26,value assigned to keyword fullscan is zero{{{17807
{{enp{{{{end procedure asign{17809
{{ejc{{{{{17810
*      asinp -- assign during pattern match
*      asinp is like asign and has a similar calling sequence
*      and effect. the difference is that the global pattern
*      variables are saved and restored if required.
*      (xl)                  base pointer for variable
*      (wa)                  offset for variable
*      (wb)                  value to be assigned
*      jsr  asinp            call to assign value to variable
*      ppm  loc              transfer loc if failure
*      (xr,xl)               destroyed
*      (wa,wb,wc,ra)         destroyed
{asinp{prc{25,r{1,1{{entry point, recursive{17826
{{add{7,xl{8,wa{{point to variable{17827
{{mov{7,xr{9,(xl){{load current contents{17828
{{beq{9,(xr){22,=b_trt{6,asnp1{jump if trapped{17829
{{mov{9,(xl){8,wb{{else perform assignment{17830
{{zer{7,xl{{{clear garbage value in xl{17831
{{exi{{{{return to asinp caller{17832
*      here if variable is trapped
{asnp1{sub{7,xl{8,wa{{restore base pointer{17836
{{mov{11,-(xs){3,pmssl{{stack subject string length{17837
{{mov{11,-(xs){3,pmhbs{{stack history stack base ptr{17838
{{mov{11,-(xs){3,r_pms{{stack subject string pointer{17839
{{mov{11,-(xs){3,pmdfl{{stack dot flag{17840
{{jsr{6,asign{{{call full-blown assignment routine{17841
{{ppm{6,asnp2{{{jump if failure{17842
{{mov{3,pmdfl{10,(xs)+{{restore dot flag{17843
{{mov{3,r_pms{10,(xs)+{{restore subject string pointer{17844
{{mov{3,pmhbs{10,(xs)+{{restore history stack base pointer{17845
{{mov{3,pmssl{10,(xs)+{{restore subject string length{17846
{{exi{{{{return to asinp caller{17847
*      here if failure in asign call
{asnp2{mov{3,pmdfl{10,(xs)+{{restore dot flag{17851
{{mov{3,r_pms{10,(xs)+{{restore subject string pointer{17852
{{mov{3,pmhbs{10,(xs)+{{restore history stack base pointer{17853
{{mov{3,pmssl{10,(xs)+{{restore subject string length{17854
{{exi{1,1{{{take failure exit{17855
{{enp{{{{end procedure asinp{17856
{{ejc{{{{{17857
*      blkln -- determine length of block
*      blkln determines the length of a block in dynamic store.
*      (wa)                  first word of block
*      (xr)                  pointer to block
*      jsr  blkln            call to get block length
*      (wa)                  length of block in bytes
*      (xl)                  destroyed
*      blkln is used by the garbage collector and is not
*      permitted to call gbcol directly or indirectly.
*      the first word stored in the block (i.e. at xr) may
*      be anything, but the contents of wa must be correct.
{blkln{prc{25,e{1,0{{entry point{17875
{{mov{7,xl{8,wa{{copy first word{17876
{{lei{7,xl{{{get entry id (bl_xx){17877
{{bsw{7,xl{2,bl___{6,bln00{switch on block type{17878
{{iff{2,bl_ar{6,bln01{{arblk{17918
{{iff{2,bl_cd{6,bln12{{cdblk{17918
{{iff{2,bl_ex{6,bln12{{exblk{17918
{{iff{2,bl_ic{6,bln07{{icblk{17918
{{iff{2,bl_nm{6,bln03{{nmblk{17918
{{iff{2,bl_p0{6,bln02{{p0blk{17918
{{iff{2,bl_p1{6,bln03{{p1blk{17918
{{iff{2,bl_p2{6,bln04{{p2blk{17918
{{iff{2,bl_rc{6,bln09{{rcblk{17918
{{iff{2,bl_sc{6,bln10{{scblk{17918
{{iff{2,bl_se{6,bln02{{seblk{17918
{{iff{2,bl_tb{6,bln01{{tbblk{17918
{{iff{2,bl_vc{6,bln01{{vcblk{17918
{{iff{1,13{6,bln00{{{17918
{{iff{1,14{6,bln00{{{17918
{{iff{1,15{6,bln00{{{17918
{{iff{2,bl_pd{6,bln08{{pdblk{17918
{{iff{2,bl_tr{6,bln05{{trblk{17918
{{iff{1,18{6,bln00{{{17918
{{iff{1,19{6,bln00{{{17918
{{iff{1,20{6,bln00{{{17918
{{iff{2,bl_ct{6,bln06{{ctblk{17918
{{iff{2,bl_df{6,bln01{{dfblk{17918
{{iff{2,bl_ef{6,bln01{{efblk{17918
{{iff{2,bl_ev{6,bln03{{evblk{17918
{{iff{2,bl_ff{6,bln05{{ffblk{17918
{{iff{2,bl_kv{6,bln03{{kvblk{17918
{{iff{2,bl_pf{6,bln01{{pfblk{17918
{{iff{2,bl_te{6,bln04{{teblk{17918
{{esw{{{{end of jump table on block type{17918
{{ejc{{{{{17919
*      blkln (continued)
*      here for blocks with length in second word
{bln00{mov{8,wa{13,num01(xr){{load length{17925
{{exi{{{{return to blkln caller{17926
*      here for length in third word (ar,cd,df,ef,ex,pf,tb,vc)
{bln01{mov{8,wa{13,num02(xr){{load length from third word{17930
{{exi{{{{return to blkln caller{17931
*      here for two word blocks (p0,se)
{bln02{mov{8,wa{19,*num02{{load length (two words){17935
{{exi{{{{return to blkln caller{17936
*      here for three word blocks (nm,p1,ev,kv)
{bln03{mov{8,wa{19,*num03{{load length (three words){17940
{{exi{{{{return to blkln caller{17941
*      here for four word blocks (p2,te,bc)
{bln04{mov{8,wa{19,*num04{{load length (four words){17945
{{exi{{{{return to blkln caller{17946
*      here for five word blocks (ff,tr)
{bln05{mov{8,wa{19,*num05{{load length{17950
{{exi{{{{return to blkln caller{17951
{{ejc{{{{{17952
*      blkln (continued)
*      here for ctblk
{bln06{mov{8,wa{19,*ctsi_{{set size of ctblk{17958
{{exi{{{{return to blkln caller{17959
*      here for icblk
{bln07{mov{8,wa{19,*icsi_{{set size of icblk{17963
{{exi{{{{return to blkln caller{17964
*      here for pdblk
{bln08{mov{7,xl{13,pddfp(xr){{point to dfblk{17968
{{mov{8,wa{13,dfpdl(xl){{load pdblk length from dfblk{17969
{{exi{{{{return to blkln caller{17970
*      here for rcblk
{bln09{mov{8,wa{19,*rcsi_{{set size of rcblk{17976
{{exi{{{{return to blkln caller{17977
*      here for scblk
{bln10{mov{8,wa{13,sclen(xr){{load length in characters{17982
{{ctb{8,wa{2,scsi_{{calculate length in bytes{17983
{{exi{{{{return to blkln caller{17984
*      here for length in fourth word (cd,ex)
{bln12{mov{8,wa{13,num03(xr){{load length from cdlen/exlen{17998
{{exi{{{{return to blkln caller{17999
{{enp{{{{end procedure blkln{18001
{{ejc{{{{{18002
*      copyb -- copy a block
*      (xs)                  block to be copied
*      jsr  copyb            call to copy block
*      ppm  loc              return if block has no idval field
*                            normal return if idval field
*      (xr)                  copy of block
*      (xs)                  popped
*      (xl,wa,wb,wc)         destroyed
{copyb{prc{25,n{1,1{{entry point{18014
{{mov{7,xr{9,(xs){{load argument{18015
{{beq{7,xr{21,=nulls{6,cop10{return argument if it is null{18016
{{mov{8,wa{9,(xr){{else load type word{18017
{{mov{8,wb{8,wa{{copy type word{18018
{{jsr{6,blkln{{{get length of argument block{18019
{{mov{7,xl{7,xr{{copy pointer{18020
{{jsr{6,alloc{{{allocate block of same size{18021
{{mov{9,(xs){7,xr{{store pointer to copy{18022
{{mvw{{{{copy contents of old block to new{18023
{{zer{7,xl{{{clear garbage xl{18024
{{mov{7,xr{9,(xs){{reload pointer to start of copy{18025
{{beq{8,wb{22,=b_tbt{6,cop05{jump if table{18026
{{beq{8,wb{22,=b_vct{6,cop01{jump if vector{18027
{{beq{8,wb{22,=b_pdt{6,cop01{jump if program defined{18028
{{bne{8,wb{22,=b_art{6,cop10{return copy if not array{18033
*      here for array (arblk)
{{add{7,xr{13,arofs(xr){{point to prototype field{18037
{{brn{6,cop02{{{jump to merge{18038
*      here for vector, program defined
{cop01{add{7,xr{19,*pdfld{{point to pdfld = vcvls{18042
*      merge here for arblk, vcblk, pdblk to delete trap
*      blocks from all value fields (the copy is untrapped)
{cop02{mov{7,xl{9,(xr){{load next pointer{18047
*      loop to get value at end of trblk chain
{cop03{bne{9,(xl){22,=b_trt{6,cop04{jump if not trapped{18051
{{mov{7,xl{13,trval(xl){{else point to next value{18052
{{brn{6,cop03{{{and loop back{18053
{{ejc{{{{{18054
*      copyb (continued)
*      here with untrapped value in xl
{cop04{mov{10,(xr)+{7,xl{{store real value, bump pointer{18060
{{bne{7,xr{3,dnamp{6,cop02{loop back if more to go{18061
{{brn{6,cop09{{{else jump to exit{18062
*      here to copy a table
{cop05{zer{13,idval(xr){{{zero id to stop dump blowing up{18066
{{mov{8,wa{19,*tesi_{{set size of teblk{18067
{{mov{8,wc{19,*tbbuk{{set initial offset{18068
*      loop through buckets in table
{cop06{mov{7,xr{9,(xs){{load table pointer{18072
{{beq{8,wc{13,tblen(xr){6,cop09{jump to exit if all done{18073
{{mov{8,wb{8,wc{{else copy offset{18074
{{sub{8,wb{19,*tenxt{{subtract link offset to merge{18075
{{add{7,xr{8,wb{{next bucket header less link offset{18076
{{ica{8,wc{{{bump offset{18077
*      loop through teblks on one chain
{cop07{mov{7,xl{13,tenxt(xr){{load pointer to next teblk{18081
{{mov{13,tenxt(xr){9,(xs){{set end of chain pointer in case{18082
{{beq{9,(xl){22,=b_tbt{6,cop06{back for next bucket if chain end{18083
{{sub{7,xr{8,wb{{point to head of previous block{18084
{{mov{11,-(xs){7,xr{{stack ptr to previous block{18085
{{mov{8,wa{19,*tesi_{{set size of teblk{18086
{{jsr{6,alloc{{{allocate new teblk{18087
{{mov{11,-(xs){7,xr{{stack ptr to new teblk{18088
{{mvw{{{{copy old teblk to new teblk{18089
{{mov{7,xr{10,(xs)+{{restore pointer to new teblk{18090
{{mov{7,xl{10,(xs)+{{restore pointer to previous block{18091
{{add{7,xl{8,wb{{add offset back in{18092
{{mov{13,tenxt(xl){7,xr{{link new block to previous{18093
{{mov{7,xl{7,xr{{copy pointer to new block{18094
*      loop to set real value after removing trap chain
{cop08{mov{7,xl{13,teval(xl){{load value{18098
{{beq{9,(xl){22,=b_trt{6,cop08{loop back if trapped{18099
{{mov{13,teval(xr){7,xl{{store untrapped value in teblk{18100
{{zer{8,wb{{{zero offset within teblk{18101
{{brn{6,cop07{{{back for next teblk{18102
*      common exit point
{cop09{mov{7,xr{10,(xs)+{{load pointer to block{18106
{{exi{{{{return{18107
*      alternative return
{cop10{exi{1,1{{{return{18111
{{ejc{{{{{18112
{{enp{{{{end procedure copyb{18130
*      cdgcg -- generate code for complex goto
*      used by cmpil to process complex goto tree
*      (wb)                  must be collectable
*      (xr)                  expression pointer
*      jsr  cdgcg            call to generate complex goto
*      (xl,xr,wa)            destroyed
{cdgcg{prc{25,e{1,0{{entry point{18141
{{mov{7,xl{13,cmopn(xr){{get unary goto operator{18142
{{mov{7,xr{13,cmrop(xr){{point to goto operand{18143
{{beq{7,xl{21,=opdvd{6,cdgc2{jump if direct goto{18144
{{jsr{6,cdgnm{{{generate opnd by name if not direct{18145
*      return point
{cdgc1{mov{8,wa{7,xl{{goto operator{18149
{{jsr{6,cdwrd{{{generate it{18150
{{exi{{{{return to caller{18151
*      direct goto
{cdgc2{jsr{6,cdgvl{{{generate operand by value{18155
{{brn{6,cdgc1{{{merge to return{18156
{{enp{{{{end procedure cdgcg{18157
{{ejc{{{{{18158
*      cdgex -- build expression block
*      cdgex is passed a pointer to an expression tree (see
*      expan) and returns an expression (seblk or exblk).
*      (wa)                  0 if by value, 1 if by name
*      (wc)                  some collectable value
*      (wb)                  integer in range 0 le x le mxlen
*      (xl)                  ptr to expression tree
*      jsr  cdgex            call to build expression
*      (xr)                  ptr to seblk or exblk
*      (xl,wa,wb)            destroyed
{cdgex{prc{25,r{1,0{{entry point, recursive{18175
{{blo{9,(xl){22,=b_vr_{6,cdgx1{jump if not variable{18176
*      here for natural variable, build seblk
{{mov{8,wa{19,*sesi_{{set size of seblk{18180
{{jsr{6,alloc{{{allocate space for seblk{18181
{{mov{9,(xr){22,=b_sel{{set type word{18182
{{mov{13,sevar(xr){7,xl{{store vrblk pointer{18183
{{exi{{{{return to cdgex caller{18184
*      here if not variable, build exblk
{cdgx1{mov{7,xr{7,xl{{copy tree pointer{18188
{{mov{11,-(xs){8,wc{{save wc{18189
{{mov{7,xl{3,cwcof{{save current offset{18190
{{bze{8,wa{6,cdgx2{{jump if by value{18192
{{mov{8,wa{9,(xr){{get type word{18194
{{bne{8,wa{22,=b_cmt{6,cdgx2{call by value if not cmblk{18195
{{bge{13,cmtyp(xr){18,=c__nm{6,cdgx2{jump if cmblk only by value{18196
{{ejc{{{{{18197
*      cdgex (continued)
*      here if expression can be evaluated by name
{{jsr{6,cdgnm{{{generate code by name{18203
{{mov{8,wa{21,=ornm_{{load return by name word{18204
{{brn{6,cdgx3{{{merge with value case{18205
*      here if expression can only be evaluated by value
{cdgx2{jsr{6,cdgvl{{{generate code by value{18209
{{mov{8,wa{21,=orvl_{{load return by value word{18210
*      merge here to construct exblk
{cdgx3{jsr{6,cdwrd{{{generate return word{18214
{{jsr{6,exbld{{{build exblk{18215
{{mov{8,wc{10,(xs)+{{restore wc{18216
{{exi{{{{return to cdgex caller{18217
{{enp{{{{end procedure cdgex{18218
{{ejc{{{{{18219
*      cdgnm -- generate code by name
*      cdgnm is called during the compilation process to
*      generate code by name for an expression. see cdblk
*      description for details of code generated. the input
*      to cdgnm is an expression tree as generated by expan.
*      cdgnm is a recursive procedure which proceeds by making
*      recursive calls to generate code for operands.
*      (wb)                  integer in range 0 le n le dnamb
*      (xr)                  ptr to tree generated by expan
*      (wc)                  constant flag (see below)
*      jsr  cdgnm            call to generate code by name
*      (xr,wa)               destroyed
*      (wc)                  set non-zero if non-constant
*      wc is set to a non-zero (collectable) value if the
*      expression for which code is generated cannot be
*      evaluated at compile time, otherwise wc is unchanged.
*      the code is generated in the current ccblk (see cdwrd).
{cdgnm{prc{25,r{1,0{{entry point, recursive{18244
{{mov{11,-(xs){7,xl{{save entry xl{18245
{{mov{11,-(xs){8,wb{{save entry wb{18246
{{chk{{{{check for stack overflow{18247
{{mov{8,wa{9,(xr){{load type word{18248
{{beq{8,wa{22,=b_cmt{6,cgn04{jump if cmblk{18249
{{bhi{8,wa{22,=b_vr_{6,cgn02{jump if simple variable{18250
*      merge here for operand yielding value (e.g. constant)
{cgn01{erb{1,212{26,syntax error: value used where name is required{{{18254
*      here for natural variable reference
{cgn02{mov{8,wa{21,=olvn_{{load variable load call{18258
{{jsr{6,cdwrd{{{generate it{18259
{{mov{8,wa{7,xr{{copy vrblk pointer{18260
{{jsr{6,cdwrd{{{generate vrblk pointer{18261
{{ejc{{{{{18262
*      cdgnm (continued)
*      here to exit with wc set correctly
{cgn03{mov{8,wb{10,(xs)+{{restore entry wb{18268
{{mov{7,xl{10,(xs)+{{restore entry xl{18269
{{exi{{{{return to cdgnm caller{18270
*      here for cmblk
{cgn04{mov{7,xl{7,xr{{copy cmblk pointer{18274
{{mov{7,xr{13,cmtyp(xr){{load cmblk type{18275
{{bge{7,xr{18,=c__nm{6,cgn01{error if not name operand{18276
{{bsw{7,xr{2,c__nm{{else switch on type{18277
{{iff{2,c_arr{6,cgn05{{array reference{18285
{{iff{2,c_fnc{6,cgn08{{function call{18285
{{iff{2,c_def{6,cgn09{{deferred expression{18285
{{iff{2,c_ind{6,cgn10{{indirect reference{18285
{{iff{2,c_key{6,cgn11{{keyword reference{18285
{{iff{2,c_ubo{6,cgn08{{undefined binary op{18285
{{iff{2,c_uuo{6,cgn08{{undefined unary op{18285
{{esw{{{{end switch on cmblk type{18285
*      here to generate code for array reference
{cgn05{mov{8,wb{19,*cmopn{{point to array operand{18289
*      loop to generate code for array operand and subscripts
{cgn06{jsr{6,cmgen{{{generate code for next operand{18293
{{mov{8,wc{13,cmlen(xl){{load length of cmblk{18294
{{blt{8,wb{8,wc{6,cgn06{loop till all generated{18295
*      generate appropriate array call
{{mov{8,wa{21,=oaon_{{load one-subscript case call{18299
{{beq{8,wc{19,*cmar1{6,cgn07{jump to exit if one subscript case{18300
{{mov{8,wa{21,=oamn_{{else load multi-subscript case call{18301
{{jsr{6,cdwrd{{{generate call{18302
{{mov{8,wa{8,wc{{copy cmblk length{18303
{{btw{8,wa{{{convert to words{18304
{{sub{8,wa{18,=cmvls{{calculate number of subscripts{18305
{{ejc{{{{{18306
*      cdgnm (continued)
*      here to exit generating word (non-constant)
{cgn07{mnz{8,wc{{{set result non-constant{18312
{{jsr{6,cdwrd{{{generate word{18313
{{brn{6,cgn03{{{back to exit{18314
*      here to generate code for functions and undefined oprs
{cgn08{mov{7,xr{7,xl{{copy cmblk pointer{18318
{{jsr{6,cdgvl{{{gen code by value for call{18319
{{mov{8,wa{21,=ofne_{{get extra call for by name{18320
{{brn{6,cgn07{{{back to generate and exit{18321
*      here to generate code for defered expression
{cgn09{mov{7,xr{13,cmrop(xl){{check if variable{18325
{{bhi{9,(xr){22,=b_vr_{6,cgn02{treat *variable as simple var{18326
{{mov{7,xl{7,xr{{copy ptr to expression tree{18327
{{mov{8,wa{18,=num01{{return name{18329
{{jsr{6,cdgex{{{else build exblk{18331
{{mov{8,wa{21,=olex_{{set call to load expr by name{18332
{{jsr{6,cdwrd{{{generate it{18333
{{mov{8,wa{7,xr{{copy exblk pointer{18334
{{jsr{6,cdwrd{{{generate exblk pointer{18335
{{brn{6,cgn03{{{back to exit{18336
*      here to generate code for indirect reference
{cgn10{mov{7,xr{13,cmrop(xl){{get operand{18340
{{jsr{6,cdgvl{{{generate code by value for it{18341
{{mov{8,wa{21,=oinn_{{load call for indirect by name{18342
{{brn{6,cgn12{{{merge{18343
*      here to generate code for keyword reference
{cgn11{mov{7,xr{13,cmrop(xl){{get operand{18347
{{jsr{6,cdgnm{{{generate code by name for it{18348
{{mov{8,wa{21,=okwn_{{load call for keyword by name{18349
*      keyword, indirect merge here
{cgn12{jsr{6,cdwrd{{{generate code for operator{18353
{{brn{6,cgn03{{{exit{18354
{{enp{{{{end procedure cdgnm{18355
{{ejc{{{{{18356
*      cdgvl -- generate code by value
*      cdgvl is called during the compilation process to
*      generate code by value for an expression. see cdblk
*      description for details of the code generated. the input
*      to cdgvl is an expression tree as generated by expan.
*      cdgvl is a recursive procedure which proceeds by making
*      recursive calls to generate code for operands.
*      (wb)                  integer in range 0 le n le dnamb
*      (xr)                  ptr to tree generated by expan
*      (wc)                  constant flag (see below)
*      jsr  cdgvl            call to generate code by value
*      (xr,wa)               destroyed
*      (wc)                  set non-zero if non-constant
*      wc is set to a non-zero (collectable) value if the
*      expression for which code is generated cannot be
*      evaluated at compile time, otherwise wc is unchanged.
*      if wc is non-zero on entry, then preevaluation is not
*      allowed regardless of the nature of the operand.
*      the code is generated in the current ccblk (see cdwrd).
{cdgvl{prc{25,r{1,0{{entry point, recursive{18384
{{mov{8,wa{9,(xr){{load type word{18385
{{beq{8,wa{22,=b_cmt{6,cgv01{jump if cmblk{18386
{{blt{8,wa{22,=b_vra{6,cgv00{jump if icblk, rcblk, scblk{18387
{{bnz{13,vrlen(xr){6,cgvl0{{jump if not system variable{18388
{{mov{11,-(xs){7,xr{{stack xr{18389
{{mov{7,xr{13,vrsvp(xr){{point to svblk{18390
{{mov{8,wa{13,svbit(xr){{get svblk property bits{18391
{{mov{7,xr{10,(xs)+{{recover xr{18392
{{anb{8,wa{4,btkwv{{check if constant keyword value{18393
{{beq{8,wa{4,btkwv{6,cgv00{jump if constant keyword value{18394
*      here for variable value reference
{cgvl0{mnz{8,wc{{{indicate non-constant value{18398
*      merge here for simple constant (icblk,rcblk,scblk)
*      and for variables corresponding to constant keywords.
{cgv00{mov{8,wa{7,xr{{copy ptr to var or constant{18403
{{jsr{6,cdwrd{{{generate as code word{18404
{{exi{{{{return to caller{18405
{{ejc{{{{{18406
*      cdgvl (continued)
*      here for tree node (cmblk)
{cgv01{mov{11,-(xs){8,wb{{save entry wb{18412
{{mov{11,-(xs){7,xl{{save entry xl{18413
{{mov{11,-(xs){8,wc{{save entry constant flag{18414
{{mov{11,-(xs){3,cwcof{{save initial code offset{18415
{{chk{{{{check for stack overflow{18416
*      prepare to generate code for cmblk. wc is set to the
*      value of cswno (zero if -optimise, 1 if -noopt) to
*      start with and is reset non-zero for any non-constant
*      code generated. if it is still zero after generating all
*      the cmblk code, then its value is computed as the result.
{{mov{7,xl{7,xr{{copy cmblk pointer{18424
{{mov{7,xr{13,cmtyp(xr){{load cmblk type{18425
{{mov{8,wc{3,cswno{{reset constant flag{18426
{{ble{7,xr{18,=c_pr_{6,cgv02{jump if not predicate value{18427
{{mnz{8,wc{{{else force non-constant case{18428
*      here with wc set appropriately
{cgv02{bsw{7,xr{2,c__nv{{switch to appropriate generator{18432
{{iff{2,c_arr{6,cgv03{{array reference{18452
{{iff{2,c_fnc{6,cgv05{{function call{18452
{{iff{2,c_def{6,cgv14{{deferred expression{18452
{{iff{2,c_ind{6,cgv31{{indirect reference{18452
{{iff{2,c_key{6,cgv27{{keyword reference{18452
{{iff{2,c_ubo{6,cgv29{{undefined binop{18452
{{iff{2,c_uuo{6,cgv30{{undefined unop{18452
{{iff{2,c_bvl{6,cgv18{{binops with val opds{18452
{{iff{2,c_uvl{6,cgv19{{unops with valu opnd{18452
{{iff{2,c_alt{6,cgv18{{alternation{18452
{{iff{2,c_cnc{6,cgv24{{concatenation{18452
{{iff{2,c_cnp{6,cgv24{{concatenation (not pattern match){18452
{{iff{2,c_unm{6,cgv27{{unops with name opnd{18452
{{iff{2,c_bvn{6,cgv26{{binary _ and .{18452
{{iff{2,c_ass{6,cgv21{{assignment{18452
{{iff{2,c_int{6,cgv31{{interrogation{18452
{{iff{2,c_neg{6,cgv28{{negation{18452
{{iff{2,c_sel{6,cgv15{{selection{18452
{{iff{2,c_pmt{6,cgv18{{pattern match{18452
{{esw{{{{end switch on cmblk type{18452
{{ejc{{{{{18453
*      cdgvl (continued)
*      here to generate code for array reference
{cgv03{mov{8,wb{19,*cmopn{{set offset to array operand{18459
*      loop to generate code for array operand and subscripts
{cgv04{jsr{6,cmgen{{{gen value code for next operand{18463
{{mov{8,wc{13,cmlen(xl){{load cmblk length{18464
{{blt{8,wb{8,wc{6,cgv04{loop back if more to go{18465
*      generate call to appropriate array reference routine
{{mov{8,wa{21,=oaov_{{set one subscript call in case{18469
{{beq{8,wc{19,*cmar1{6,cgv32{jump to exit if 1-sub case{18470
{{mov{8,wa{21,=oamv_{{else set call for multi-subscripts{18471
{{jsr{6,cdwrd{{{generate call{18472
{{mov{8,wa{8,wc{{copy length of cmblk{18473
{{sub{8,wa{19,*cmvls{{subtract standard length{18474
{{btw{8,wa{{{get number of words{18475
{{brn{6,cgv32{{{jump to generate subscript count{18476
*      here to generate code for function call
{cgv05{mov{8,wb{19,*cmvls{{set offset to first argument{18480
*      loop to generate code for arguments
{cgv06{beq{8,wb{13,cmlen(xl){6,cgv07{jump if all generated{18484
{{jsr{6,cmgen{{{else gen value code for next arg{18485
{{brn{6,cgv06{{{back to generate next argument{18486
*      here to generate actual function call
{cgv07{sub{8,wb{19,*cmvls{{get number of arg ptrs (bytes){18490
{{btw{8,wb{{{convert bytes to words{18491
{{mov{7,xr{13,cmopn(xl){{load function vrblk pointer{18492
{{bnz{13,vrlen(xr){6,cgv12{{jump if not system function{18493
{{mov{7,xl{13,vrsvp(xr){{load svblk ptr if system var{18494
{{mov{8,wa{13,svbit(xl){{load bit mask{18495
{{anb{8,wa{4,btffc{{test for fast function call allowed{18496
{{zrb{8,wa{6,cgv12{{jump if not{18497
{{ejc{{{{{18498
*      cdgvl (continued)
*      here if fast function call is allowed
{{mov{8,wa{13,svbit(xl){{reload bit indicators{18504
{{anb{8,wa{4,btpre{{test for preevaluation ok{18505
{{nzb{8,wa{6,cgv08{{jump if preevaluation permitted{18506
{{mnz{8,wc{{{else set result non-constant{18507
*      test for correct number of args for fast call
{cgv08{mov{7,xl{13,vrfnc(xr){{load ptr to svfnc field{18511
{{mov{8,wa{13,fargs(xl){{load svnar field value{18512
{{beq{8,wa{8,wb{6,cgv11{jump if argument count is correct{18513
{{bhi{8,wa{8,wb{6,cgv09{jump if too few arguments given{18514
*      here if too many arguments, prepare to generate o_pops
{{sub{8,wb{8,wa{{get number of extra args{18518
{{lct{8,wb{8,wb{{set as count to control loop{18519
{{mov{8,wa{21,=opop_{{set pop call{18520
{{brn{6,cgv10{{{jump to common loop{18521
*      here if too few arguments, prepare to generate nulls
{cgv09{sub{8,wa{8,wb{{get number of missing arguments{18525
{{lct{8,wb{8,wa{{load as count to control loop{18526
{{mov{8,wa{21,=nulls{{load ptr to null constant{18527
*      loop to generate calls to fix argument count
{cgv10{jsr{6,cdwrd{{{generate one call{18531
{{bct{8,wb{6,cgv10{{loop till all generated{18532
*      here after adjusting arg count as required
{cgv11{mov{8,wa{7,xl{{copy pointer to svfnc field{18536
{{brn{6,cgv36{{{jump to generate call{18537
{{ejc{{{{{18538
*      cdgvl (continued)
*      come here if fast call is not permitted
{cgv12{mov{8,wa{21,=ofns_{{set one arg call in case{18544
{{beq{8,wb{18,=num01{6,cgv13{jump if one arg case{18545
{{mov{8,wa{21,=ofnc_{{else load call for more than 1 arg{18546
{{jsr{6,cdwrd{{{generate it{18547
{{mov{8,wa{8,wb{{copy argument count{18548
*      one arg case merges here
{cgv13{jsr{6,cdwrd{{{generate =o_fns or arg count{18552
{{mov{8,wa{7,xr{{copy vrblk pointer{18553
{{brn{6,cgv32{{{jump to generate vrblk ptr{18554
*      here for deferred expression
{cgv14{mov{7,xl{13,cmrop(xl){{point to expression tree{18558
{{zer{8,wa{{{return value{18560
{{jsr{6,cdgex{{{build exblk or seblk{18562
{{mov{8,wa{7,xr{{copy block ptr{18563
{{jsr{6,cdwrd{{{generate ptr to exblk or seblk{18564
{{brn{6,cgv34{{{jump to exit, constant test{18565
*      here to generate code for selection
{cgv15{zer{11,-(xs){{{zero ptr to chain of forward jumps{18569
{{zer{11,-(xs){{{zero ptr to prev o_slc forward ptr{18570
{{mov{8,wb{19,*cmvls{{point to first alternative{18571
{{mov{8,wa{21,=osla_{{set initial code word{18572
*      0(xs)                 is the offset to the previous word
*                            which requires filling in with an
*                            offset to the following o_slc,o_sld
*      1(xs)                 is the head of a chain of offset
*                            pointers indicating those locations
*                            to be filled with offsets past
*                            the end of all the alternatives
{cgv16{jsr{6,cdwrd{{{generate o_slc (o_sla first time){18583
{{mov{9,(xs){3,cwcof{{set current loc as ptr to fill in{18584
{{jsr{6,cdwrd{{{generate garbage word there for now{18585
{{jsr{6,cmgen{{{gen value code for alternative{18586
{{mov{8,wa{21,=oslb_{{load o_slb pointer{18587
{{jsr{6,cdwrd{{{generate o_slb call{18588
{{mov{8,wa{13,num01(xs){{load old chain ptr{18589
{{mov{13,num01(xs){3,cwcof{{set current loc as new chain head{18590
{{jsr{6,cdwrd{{{generate forward chain link{18591
{{ejc{{{{{18592
*      cdgvl (continued)
*      now to fill in the skip offset to o_slc,o_sld
{{mov{7,xr{9,(xs){{load offset to word to plug{18598
{{add{7,xr{3,r_ccb{{point to actual location to plug{18599
{{mov{9,(xr){3,cwcof{{plug proper offset in{18600
{{mov{8,wa{21,=oslc_{{load o_slc ptr for next alternative{18601
{{mov{7,xr{8,wb{{copy offset (destroy garbage xr){18602
{{ica{7,xr{{{bump extra time for test{18603
{{blt{7,xr{13,cmlen(xl){6,cgv16{loop back if not last alternative{18604
*      here to generate code for last alternative
{{mov{8,wa{21,=osld_{{get header call{18608
{{jsr{6,cdwrd{{{generate o_sld call{18609
{{jsr{6,cmgen{{{generate code for last alternative{18610
{{ica{7,xs{{{pop offset ptr{18611
{{mov{7,xr{10,(xs)+{{load chain ptr{18612
*      loop to plug offsets past structure
{cgv17{add{7,xr{3,r_ccb{{make next ptr absolute{18616
{{mov{8,wa{9,(xr){{load forward ptr{18617
{{mov{9,(xr){3,cwcof{{plug required offset{18618
{{mov{7,xr{8,wa{{copy forward ptr{18619
{{bnz{8,wa{6,cgv17{{loop back if more to go{18620
{{brn{6,cgv33{{{else jump to exit (not constant){18621
*      here for binary ops with value operands
{cgv18{mov{7,xr{13,cmlop(xl){{load left operand pointer{18625
{{jsr{6,cdgvl{{{gen value code for left operand{18626
*      here for unary ops with value operand (binops merge)
{cgv19{mov{7,xr{13,cmrop(xl){{load right (only) operand ptr{18630
{{jsr{6,cdgvl{{{gen code by value{18631
{{ejc{{{{{18632
*      cdgvl (continued)
*      merge here to generate operator call from cmopn field
{cgv20{mov{8,wa{13,cmopn(xl){{load operator call pointer{18638
{{brn{6,cgv36{{{jump to generate it with cons test{18639
*      here for assignment
{cgv21{mov{7,xr{13,cmlop(xl){{load left operand pointer{18643
{{blo{9,(xr){22,=b_vr_{6,cgv22{jump if not variable{18644
*      here for assignment to simple variable
{{mov{7,xr{13,cmrop(xl){{load right operand ptr{18648
{{jsr{6,cdgvl{{{generate code by value{18649
{{mov{8,wa{13,cmlop(xl){{reload left operand vrblk ptr{18650
{{add{8,wa{19,*vrsto{{point to vrsto field{18651
{{brn{6,cgv32{{{jump to generate store ptr{18652
*      here if not simple variable assignment
{cgv22{jsr{6,expap{{{test for pattern match on left side{18656
{{ppm{6,cgv23{{{jump if not pattern match{18657
*      here for pattern replacement
{{mov{13,cmlop(xl){13,cmrop(xr){{save pattern ptr in safe place{18661
{{mov{7,xr{13,cmlop(xr){{load subject ptr{18662
{{jsr{6,cdgnm{{{gen code by name for subject{18663
{{mov{7,xr{13,cmlop(xl){{load pattern ptr{18664
{{jsr{6,cdgvl{{{gen code by value for pattern{18665
{{mov{8,wa{21,=opmn_{{load match by name call{18666
{{jsr{6,cdwrd{{{generate it{18667
{{mov{7,xr{13,cmrop(xl){{load replacement value ptr{18668
{{jsr{6,cdgvl{{{gen code by value{18669
{{mov{8,wa{21,=orpl_{{load replace call{18670
{{brn{6,cgv32{{{jump to gen and exit (not constant){18671
*      here for assignment to complex variable
{cgv23{mnz{8,wc{{{inhibit pre-evaluation{18675
{{jsr{6,cdgnm{{{gen code by name for left side{18676
{{brn{6,cgv31{{{merge with unop circuit{18677
{{ejc{{{{{18678
*      cdgvl (continued)
*      here for concatenation
{cgv24{mov{7,xr{13,cmlop(xl){{load left operand ptr{18684
{{bne{9,(xr){22,=b_cmt{6,cgv18{ordinary binop if not cmblk{18685
{{mov{8,wb{13,cmtyp(xr){{load cmblk type code{18686
{{beq{8,wb{18,=c_int{6,cgv25{special case if interrogation{18687
{{beq{8,wb{18,=c_neg{6,cgv25{or negation{18688
{{bne{8,wb{18,=c_fnc{6,cgv18{else ordinary binop if not function{18689
{{mov{7,xr{13,cmopn(xr){{else load function vrblk ptr{18690
{{bnz{13,vrlen(xr){6,cgv18{{ordinary binop if not system var{18691
{{mov{7,xr{13,vrsvp(xr){{else point to svblk{18692
{{mov{8,wa{13,svbit(xr){{load bit indicators{18693
{{anb{8,wa{4,btprd{{test for predicate function{18694
{{zrb{8,wa{6,cgv18{{ordinary binop if not{18695
*      here if left arg of concatenation is predicate function
{cgv25{mov{7,xr{13,cmlop(xl){{reload left arg{18699
{{jsr{6,cdgvl{{{gen code by value{18700
{{mov{8,wa{21,=opop_{{load pop call{18701
{{jsr{6,cdwrd{{{generate it{18702
{{mov{7,xr{13,cmrop(xl){{load right operand{18703
{{jsr{6,cdgvl{{{gen code by value as result code{18704
{{brn{6,cgv33{{{exit (not constant){18705
*      here to generate code for pattern, immediate assignment
{cgv26{mov{7,xr{13,cmlop(xl){{load left operand{18709
{{jsr{6,cdgvl{{{gen code by value, merge{18710
*      here for unops with arg by name (binary _ . merge)
{cgv27{mov{7,xr{13,cmrop(xl){{load right operand ptr{18714
{{jsr{6,cdgnm{{{gen code by name for right arg{18715
{{mov{7,xr{13,cmopn(xl){{get operator code word{18716
{{bne{9,(xr){22,=o_kwv{6,cgv20{gen call unless keyword value{18717
{{ejc{{{{{18718
*      cdgvl (continued)
*      here for keyword by value. this is constant only if
*      the operand is one of the special system variables with
*      the svckw bit set to indicate a constant keyword value.
*      note that the only constant operand by name is a variable
{{bnz{8,wc{6,cgv20{{gen call if non-constant (not var){18727
{{mnz{8,wc{{{else set non-constant in case{18728
{{mov{7,xr{13,cmrop(xl){{load ptr to operand vrblk{18729
{{bnz{13,vrlen(xr){6,cgv20{{gen (non-constant) if not sys var{18730
{{mov{7,xr{13,vrsvp(xr){{else load ptr to svblk{18731
{{mov{8,wa{13,svbit(xr){{load bit mask{18732
{{anb{8,wa{4,btckw{{test for constant keyword{18733
{{zrb{8,wa{6,cgv20{{go gen if not constant{18734
{{zer{8,wc{{{else set result constant{18735
{{brn{6,cgv20{{{and jump back to generate call{18736
*      here to generate code for negation
{cgv28{mov{8,wa{21,=onta_{{get initial word{18740
{{jsr{6,cdwrd{{{generate it{18741
{{mov{8,wb{3,cwcof{{save next offset{18742
{{jsr{6,cdwrd{{{generate gunk word for now{18743
{{mov{7,xr{13,cmrop(xl){{load right operand ptr{18744
{{jsr{6,cdgvl{{{gen code by value{18745
{{mov{8,wa{21,=ontb_{{load end of evaluation call{18746
{{jsr{6,cdwrd{{{generate it{18747
{{mov{7,xr{8,wb{{copy offset to word to plug{18748
{{add{7,xr{3,r_ccb{{point to actual word to plug{18749
{{mov{9,(xr){3,cwcof{{plug word with current offset{18750
{{mov{8,wa{21,=ontc_{{load final call{18751
{{brn{6,cgv32{{{jump to generate it (not constant){18752
*      here to generate code for undefined binary operator
{cgv29{mov{7,xr{13,cmlop(xl){{load left operand ptr{18756
{{jsr{6,cdgvl{{{generate code by value{18757
{{ejc{{{{{18758
*      cdgvl (continued)
*      here to generate code for undefined unary operator
{cgv30{mov{8,wb{18,=c_uo_{{set unop code + 1{18764
{{sub{8,wb{13,cmtyp(xl){{set number of args (1 or 2){18765
*      merge here for undefined operators
{{mov{7,xr{13,cmrop(xl){{load right (only) operand pointer{18769
{{jsr{6,cdgvl{{{gen value code for right operand{18770
{{mov{7,xr{13,cmopn(xl){{load pointer to operator dv{18771
{{mov{7,xr{13,dvopn(xr){{load pointer offset{18772
{{wtb{7,xr{{{convert word offset to bytes{18773
{{add{7,xr{20,=r_uba{{point to proper function ptr{18774
{{sub{7,xr{19,*vrfnc{{set standard function offset{18775
{{brn{6,cgv12{{{merge with function call circuit{18776
*      here to generate code for interrogation, indirection
{cgv31{mnz{8,wc{{{set non constant{18780
{{brn{6,cgv19{{{merge{18781
*      here to exit generating a word, result not constant
{cgv32{jsr{6,cdwrd{{{generate word, merge{18785
*      here to exit with no word generated, not constant
{cgv33{mnz{8,wc{{{indicate result is not constant{18789
*      common exit point
{cgv34{ica{7,xs{{{pop initial code offset{18793
{{mov{8,wa{10,(xs)+{{restore old constant flag{18794
{{mov{7,xl{10,(xs)+{{restore entry xl{18795
{{mov{8,wb{10,(xs)+{{restore entry wb{18796
{{bnz{8,wc{6,cgv35{{jump if not constant{18797
{{mov{8,wc{8,wa{{else restore entry constant flag{18798
*      here to return after dealing with wc setting
{cgv35{exi{{{{return to cdgvl caller{18802
*      exit here to generate word and test for constant
{cgv36{jsr{6,cdwrd{{{generate word{18806
{{bnz{8,wc{6,cgv34{{jump to exit if not constant{18807
{{ejc{{{{{18808
*      cdgvl (continued)
*      here to preevaluate constant sub-expression
{{mov{8,wa{21,=orvl_{{load call to return value{18814
{{jsr{6,cdwrd{{{generate it{18815
{{mov{7,xl{9,(xs){{load initial code offset{18816
{{jsr{6,exbld{{{build exblk for expression{18817
{{zer{8,wb{{{set to evaluate by value{18818
{{jsr{6,evalx{{{evaluate expression{18819
{{ppm{{{{should not fail{18820
{{mov{8,wa{9,(xr){{load type word of result{18821
{{blo{8,wa{22,=p_aaa{6,cgv37{jump if not pattern{18822
{{mov{8,wa{21,=olpt_{{else load special pattern load call{18823
{{jsr{6,cdwrd{{{generate it{18824
*      merge here to generate pointer to resulting constant
{cgv37{mov{8,wa{7,xr{{copy constant pointer{18828
{{jsr{6,cdwrd{{{generate ptr{18829
{{zer{8,wc{{{set result constant{18830
{{brn{6,cgv34{{{jump back to exit{18831
{{enp{{{{end procedure cdgvl{18832
{{ejc{{{{{18833
*      cdwrd -- generate one word of code
*      cdwrd writes one word into the current code block under
*      construction. a new, larger, block is allocated if there
*      is insufficient room in the current block. cdwrd ensures
*      that there are at least four words left in the block
*      after entering the new word. this guarantees that any
*      extra space at the end can be split off as a ccblk.
*      (wa)                  word to be generated
*      jsr  cdwrd            call to generate word
{cdwrd{prc{25,e{1,0{{entry point{18851
{{mov{11,-(xs){7,xr{{save entry xr{18852
{{mov{11,-(xs){8,wa{{save code word to be generated{18853
*      merge back here after allocating larger block
{cdwd1{mov{7,xr{3,r_ccb{{load ptr to ccblk being built{18857
{{bnz{7,xr{6,cdwd2{{jump if block allocated{18858
*      here we allocate an entirely fresh block
{{mov{8,wa{19,*e_cbs{{load initial length{18862
{{jsr{6,alloc{{{allocate ccblk{18863
{{mov{9,(xr){22,=b_cct{{store type word{18864
{{mov{3,cwcof{19,*cccod{{set initial offset{18865
{{mov{13,cclen(xr){8,wa{{store block length{18866
{{zer{13,ccsln(xr){{{zero line number{18868
{{mov{3,r_ccb{7,xr{{store ptr to new block{18870
*      here we have a block we can use
{cdwd2{mov{8,wa{3,cwcof{{load current offset{18874
{{add{8,wa{19,*num05{{adjust for test (five words){18876
{{blo{8,wa{13,cclen(xr){6,cdwd4{jump if room in this block{18880
*      here if no room in current block
{{bge{8,wa{3,mxlen{6,cdwd5{jump if already at max size{18884
{{add{8,wa{19,*e_cbs{{else get new size{18885
{{mov{11,-(xs){7,xl{{save entry xl{18886
{{mov{7,xl{7,xr{{copy pointer{18887
{{blt{8,wa{3,mxlen{6,cdwd3{jump if not too large{18888
{{mov{8,wa{3,mxlen{{else reset to max allowed size{18889
{{ejc{{{{{18890
*      cdwrd (continued)
*      here with new block size in wa
{cdwd3{jsr{6,alloc{{{allocate new block{18896
{{mov{3,r_ccb{7,xr{{store pointer to new block{18897
{{mov{10,(xr)+{22,=b_cct{{store type word in new block{18898
{{mov{10,(xr)+{8,wa{{store block length{18899
{{mov{10,(xr)+{13,ccsln(xl){{copy source line number word{18901
{{add{7,xl{19,*ccuse{{point to ccuse,cccod fields in old{18903
{{mov{8,wa{9,(xl){{load ccuse value{18904
{{mvw{{{{copy useful words from old block{18905
{{mov{7,xl{10,(xs)+{{restore xl{18906
{{brn{6,cdwd1{{{merge back to try again{18907
*      here with room in current block
{cdwd4{mov{8,wa{3,cwcof{{load current offset{18911
{{ica{8,wa{{{get new offset{18912
{{mov{3,cwcof{8,wa{{store new offset{18913
{{mov{13,ccuse(xr){8,wa{{store in ccblk for gbcol{18914
{{dca{8,wa{{{restore ptr to this word{18915
{{add{7,xr{8,wa{{point to current entry{18916
{{mov{8,wa{10,(xs)+{{reload word to generate{18917
{{mov{9,(xr){8,wa{{store word in block{18918
{{mov{7,xr{10,(xs)+{{restore entry xr{18919
{{exi{{{{return to caller{18920
*      here if compiled code is too long for cdblk
{cdwd5{erb{1,213{26,syntax error: statement is too complicated.{{{18924
{{enp{{{{end procedure cdwrd{18925
{{ejc{{{{{18926
*      cmgen -- generate code for cmblk ptr
*      cmgen is a subsidiary procedure used to generate value
*      code for a cmblk ptr from the main code generators.
*      (xl)                  cmblk pointer
*      (wb)                  offset to pointer in cmblk
*      jsr  cmgen            call to generate code
*      (xr,wa)               destroyed
*      (wb)                  bumped by one word
{cmgen{prc{25,r{1,0{{entry point, recursive{18939
{{mov{7,xr{7,xl{{copy cmblk pointer{18940
{{add{7,xr{8,wb{{point to cmblk pointer{18941
{{mov{7,xr{9,(xr){{load cmblk pointer{18942
{{jsr{6,cdgvl{{{generate code by value{18943
{{ica{8,wb{{{bump offset{18944
{{exi{{{{return to caller{18945
{{enp{{{{end procedure cmgen{18946
{{ejc{{{{{18947
*      cmpil (compile source code)
*      cmpil is used to convert snobol4 source code to internal
*      form (see cdblk format). it is used both for the initial
*      compile and at run time by the code and convert functions
*      this procedure has control for the entire duration of
*      initial compilation. an error in any procedure called
*      during compilation will lead first to the error section
*      and ultimately back here for resumed compilation. the
*      re-entry points after an error are specially labelled -
*      cmpce                 resume after control card error
*      cmple                 resume after label error
*      cmpse                 resume after statement error
*      jsr  cmpil            call to compile code
*      (xr)                  ptr to cdblk for entry statement
*      (xl,wa,wb,wc,ra)      destroyed
*      the following global variables are referenced
*      cmpln                 line number of first line of
*                            statement to be compiled
*      cmpsn                 number of next statement
*                            to be compiled.
*      cswxx                 control card switch values are
*                            changed when relevant control
*                            cards are met.
*      cwcof                 offset to next word in code block
*                            being built (see cdwrd).
*      lstsn                 number of statement most recently
*                            compiled (initially set to zero).
*      r_cim                 current (initial) compiler image
*                            (zero for initial compile call)
*      r_cni                 used to point to following image.
*                            (see readr procedure).
*      scngo                 goto switch for scane procedure
*      scnil                 length of current image excluding
*                            characters removed by -input.
*      scnpt                 current scan offset, see scane.
*      scnrs                 rescan switch for scane procedure.
*      scnse                 offset (in r_cim) of most recently
*                            scanned element. set zero if not
*                            currently scanning items
{{ejc{{{{{19004
*      cmpil (continued)
*      stage               stgic  initial compile in progress
*                          stgxc  code/convert compile
*                          stgev  building exblk for eval
*                          stgxt  execute time (outside compile)
*                          stgce  initial compile after end line
*                          stgxe  execute compile after end line
*      cmpil also uses a fixed number of locations on the
*      main stack as follows. (the definitions of the actual
*      offsets are in the definitions section).
*      cmstm(xs)             pointer to expan tree for body of
*                            statement (see expan procedure).
*      cmsgo(xs)             pointer to tree representation of
*                            success goto (see procedure scngo)
*                            zero if no success goto is given
*      cmfgo(xs)             like cmsgo for failure goto.
*      cmcgo(xs)             set non-zero only if there is a
*                            conditional goto. used for -fail,
*                            -nofail code generation.
*      cmpcd(xs)             pointer to cdblk for previous
*                            statement. zero for 1st statement.
*      cmffp(xs)             set non-zero if cdfal in previous
*                            cdblk needs filling with forward
*                            pointer, else set to zero.
*      cmffc(xs)             same as cmffp for current cdblk
*      cmsop(xs)             offset to word in previous cdblk
*                            to be filled in with forward ptr
*                            to next cdblk for success goto.
*                            zero if no fill in is required.
*      cmsoc(xs)             same as cmsop for current cdblk.
*      cmlbl(xs)             pointer to vrblk for label of
*                            current statement. zero if no label
*      cmtra(xs)             pointer to cdblk for entry stmnt.
{{ejc{{{{{19052
*      cmpil (continued)
*      entry point
{cmpil{prc{25,e{1,0{{entry point{19058
{{lct{8,wb{18,=cmnen{{set number of stack work locations{19059
*      loop to initialize stack working locations
{cmp00{zer{11,-(xs){{{store a zero, make one entry{19063
{{bct{8,wb{6,cmp00{{loop back until all set{19064
{{mov{3,cmpxs{7,xs{{save stack pointer for error sec{19065
{{sss{3,cmpss{{{save s-r stack pointer if any{19066
*      loop through statements
{cmp01{mov{8,wb{3,scnpt{{set scan pointer offset{19070
{{mov{3,scnse{8,wb{{set start of element location{19071
{{mov{8,wa{21,=ocer_{{point to compile error call{19072
{{jsr{6,cdwrd{{{generate as temporary cdfal{19073
{{blt{8,wb{3,scnil{6,cmp04{jump if chars left on this image{19074
*      loop here after comment or control card
*      also special entry after control card error
{cmpce{zer{7,xr{{{clear possible garbage xr value{19079
{{bnz{3,cnind{6,cmpc2{{if within include file{19081
{{bne{3,stage{18,=stgic{6,cmp02{skip unless initial compile{19083
{cmpc2{jsr{6,readr{{{read next input image{19084
{{bze{7,xr{6,cmp09{{jump if no input available{19085
{{jsr{6,nexts{{{acquire next source image{19086
{{mov{3,lstsn{3,cmpsn{{store stmt no for use by listr{19087
{{mov{3,cmpln{3,rdcln{{store line number at start of stmt{19088
{{zer{3,scnpt{{{reset scan pointer{19089
{{brn{6,cmp04{{{go process image{19090
*      for execute time compile, permit embedded control cards
*      and comments (by skipping to next semi-colon)
{cmp02{mov{7,xr{3,r_cim{{get current image{19095
{{mov{8,wb{3,scnpt{{get current offset{19096
{{plc{7,xr{8,wb{{prepare to get chars{19097
*      skip to semi-colon
{cmp03{bge{3,scnpt{3,scnil{6,cmp09{end loop if end of image{19101
{{lch{8,wc{10,(xr)+{{get char{19102
{{icv{3,scnpt{{{advance offset{19103
{{bne{8,wc{18,=ch_sm{6,cmp03{loop if not semi-colon{19104
{{ejc{{{{{19105
*      cmpil (continued)
*      here with image available to scan. note that if the input
*      string is null, then everything is ok since null is
*      actually assembled as a word of blanks.
{cmp04{mov{7,xr{3,r_cim{{point to current image{19113
{{mov{8,wb{3,scnpt{{load current offset{19114
{{mov{8,wa{8,wb{{copy for label scan{19115
{{plc{7,xr{8,wb{{point to first character{19116
{{lch{8,wc{10,(xr)+{{load first character{19117
{{beq{8,wc{18,=ch_sm{6,cmp12{no label if semicolon{19118
{{beq{8,wc{18,=ch_as{6,cmpce{loop back if comment card{19119
{{beq{8,wc{18,=ch_mn{6,cmp32{jump if control card{19120
{{mov{3,r_cmp{3,r_cim{{about to destroy r_cim{19121
{{mov{7,xl{20,=cmlab{{point to label work string{19122
{{mov{3,r_cim{7,xl{{scane is to scan work string{19123
{{psc{7,xl{{{point to first character position{19124
{{sch{8,wc{10,(xl)+{{store char just loaded{19125
{{mov{8,wc{18,=ch_sm{{get a semicolon{19126
{{sch{8,wc{9,(xl){{store after first char{19127
{{csc{7,xl{{{finished character storing{19128
{{zer{7,xl{{{clear pointer{19129
{{zer{3,scnpt{{{start at first character{19130
{{mov{11,-(xs){3,scnil{{preserve image length{19131
{{mov{3,scnil{18,=num02{{read 2 chars at most{19132
{{jsr{6,scane{{{scan first char for type{19133
{{mov{3,scnil{10,(xs)+{{restore image length{19134
{{mov{8,wc{7,xl{{note return code{19135
{{mov{7,xl{3,r_cmp{{get old r_cim{19136
{{mov{3,r_cim{7,xl{{put it back{19137
{{mov{3,scnpt{8,wb{{reinstate offset{19138
{{bnz{3,scnbl{6,cmp12{{blank seen - cant be label{19139
{{mov{7,xr{7,xl{{point to current image{19140
{{plc{7,xr{8,wb{{point to first char again{19141
{{beq{8,wc{18,=t_var{6,cmp06{ok if letter{19142
{{beq{8,wc{18,=t_con{6,cmp06{ok if digit{19143
*      drop in or jump from error section if scane failed
{cmple{mov{3,r_cim{3,r_cmp{{point to bad line{19147
{{erb{1,214{26,bad label or misplaced continuation line{{{19148
*      loop to scan label
{cmp05{beq{8,wc{18,=ch_sm{6,cmp07{skip if semicolon{19152
{{icv{8,wa{{{bump offset{19153
{{beq{8,wa{3,scnil{6,cmp07{jump if end of image (label end){19154
{{ejc{{{{{19155
*      cmpil (continued)
*      enter loop at this point
{cmp06{lch{8,wc{10,(xr)+{{else load next character{19161
{{beq{8,wc{18,=ch_ht{6,cmp07{jump if horizontal tab{19163
{{bne{8,wc{18,=ch_bl{6,cmp05{loop back if non-blank{19168
*      here after scanning out label
{cmp07{mov{3,scnpt{8,wa{{save updated scan offset{19172
{{sub{8,wa{8,wb{{get length of label{19173
{{bze{8,wa{6,cmp12{{skip if label length zero{19174
{{zer{7,xr{{{clear garbage xr value{19175
{{jsr{6,sbstr{{{build scblk for label name{19176
{{jsr{6,gtnvr{{{locate/contruct vrblk{19177
{{ppm{{{{dummy (impossible) error return{19178
{{mov{13,cmlbl(xs){7,xr{{store label pointer{19179
{{bnz{13,vrlen(xr){6,cmp11{{jump if not system label{19180
{{bne{13,vrsvp(xr){21,=v_end{6,cmp11{jump if not end label{19181
*      here for end label scanned out
{{add{3,stage{18,=stgnd{{adjust stage appropriately{19185
{{jsr{6,scane{{{scan out next element{19186
{{beq{7,xl{18,=t_smc{6,cmp10{jump if end of image{19187
{{bne{7,xl{18,=t_var{6,cmp08{else error if not variable{19188
*      here check for valid initial transfer
{{beq{13,vrlbl(xr){21,=stndl{6,cmp08{jump if not defined (error){19192
{{mov{13,cmtra(xs){13,vrlbl(xr){{else set initial entry pointer{19193
{{jsr{6,scane{{{scan next element{19194
{{beq{7,xl{18,=t_smc{6,cmp10{jump if ok (end of image){19195
*      here for bad transfer label
{cmp08{erb{1,215{26,syntax error: undefined or erroneous entry label{{{19199
*      here for end of input (no end label detected)
{cmp09{zer{7,xr{{{clear garbage xr value{19203
{{add{3,stage{18,=stgnd{{adjust stage appropriately{19204
{{beq{3,stage{18,=stgxe{6,cmp10{jump if code call (ok){19205
{{erb{1,216{26,syntax error: missing end line{{{19206
*      here after processing end line (merge here on end error)
{cmp10{mov{8,wa{21,=ostp_{{set stop call pointer{19210
{{jsr{6,cdwrd{{{generate as statement call{19211
{{brn{6,cmpse{{{jump to generate as failure{19212
{{ejc{{{{{19213
*      cmpil (continued)
*      here after processing label other than end
{cmp11{bne{3,stage{18,=stgic{6,cmp12{jump if code call - redef. ok{19219
{{beq{13,vrlbl(xr){21,=stndl{6,cmp12{else check for redefinition{19220
{{zer{13,cmlbl(xs){{{leave first label decln undisturbed{19221
{{erb{1,217{26,syntax error: duplicate label{{{19222
*      here after dealing with label
*      null statements and statements just containing a
*      constant subject are optimized out by resetting the
*      current ccblk to empty.
{cmp12{zer{8,wb{{{set flag for statement body{19229
{{jsr{6,expan{{{get tree for statement body{19230
{{mov{13,cmstm(xs){7,xr{{store for later use{19231
{{zer{13,cmsgo(xs){{{clear success goto pointer{19232
{{zer{13,cmfgo(xs){{{clear failure goto pointer{19233
{{zer{13,cmcgo(xs){{{clear conditional goto flag{19234
{{jsr{6,scane{{{scan next element{19235
{{beq{7,xl{18,=t_col{6,cmp13{jump if colon (goto){19236
{{bnz{3,cswno{6,cmp18{{jump if not optimizing{19237
{{bnz{13,cmlbl(xs){6,cmp18{{jump if label present{19238
{{mov{7,xr{13,cmstm(xs){{load tree ptr for statement body{19239
{{mov{8,wa{9,(xr){{load type word{19240
{{beq{8,wa{22,=b_cmt{6,cmp18{jump if cmblk{19241
{{bge{8,wa{22,=b_vra{6,cmp18{jump if not icblk, scblk, or rcblk{19242
{{mov{7,xl{3,r_ccb{{load ptr to ccblk{19243
{{mov{13,ccuse(xl){19,*cccod{{reset use offset in ccblk{19244
{{mov{3,cwcof{19,*cccod{{and in global{19245
{{icv{3,cmpsn{{{bump statement number{19246
{{brn{6,cmp01{{{generate no code for statement{19247
*      loop to process goto fields
{cmp13{mnz{3,scngo{{{set goto flag{19251
{{jsr{6,scane{{{scan next element{19252
{{beq{7,xl{18,=t_smc{6,cmp31{jump if no fields left{19253
{{beq{7,xl{18,=t_sgo{6,cmp14{jump if s for success goto{19254
{{beq{7,xl{18,=t_fgo{6,cmp16{jump if f for failure goto{19255
*      here for unconditional goto (i.e. not f or s)
{{mnz{3,scnrs{{{set to rescan element not f,s{19259
{{jsr{6,scngf{{{scan out goto field{19260
{{bnz{13,cmfgo(xs){6,cmp17{{error if fgoto already{19261
{{mov{13,cmfgo(xs){7,xr{{else set as fgoto{19262
{{brn{6,cmp15{{{merge with sgoto circuit{19263
*      here for success goto
{cmp14{jsr{6,scngf{{{scan success goto field{19267
{{mov{13,cmcgo(xs){18,=num01{{set conditional goto flag{19268
*      uncontional goto merges here
{cmp15{bnz{13,cmsgo(xs){6,cmp17{{error if sgoto already given{19272
{{mov{13,cmsgo(xs){7,xr{{else set sgoto{19273
{{brn{6,cmp13{{{loop back for next goto field{19274
*      here for failure goto
{cmp16{jsr{6,scngf{{{scan goto field{19278
{{mov{13,cmcgo(xs){18,=num01{{set conditonal goto flag{19279
{{bnz{13,cmfgo(xs){6,cmp17{{error if fgoto already given{19280
{{mov{13,cmfgo(xs){7,xr{{else store fgoto pointer{19281
{{brn{6,cmp13{{{loop back for next field{19282
{{ejc{{{{{19283
*      cmpil (continued)
*      here for duplicated goto field
{cmp17{erb{1,218{26,syntax error: duplicated goto field{{{19289
*      here to generate code
{cmp18{zer{3,scnse{{{stop positional error flags{19293
{{mov{7,xr{13,cmstm(xs){{load tree ptr for statement body{19294
{{zer{8,wb{{{collectable value for wb for cdgvl{19295
{{zer{8,wc{{{reset constant flag for cdgvl{19296
{{jsr{6,expap{{{test for pattern match{19297
{{ppm{6,cmp19{{{jump if not pattern match{19298
{{mov{13,cmopn(xr){21,=opms_{{else set pattern match pointer{19299
{{mov{13,cmtyp(xr){18,=c_pmt{{{19300
*      here after dealing with special pattern match case
{cmp19{jsr{6,cdgvl{{{generate code for body of statement{19304
{{mov{7,xr{13,cmsgo(xs){{load sgoto pointer{19305
{{mov{8,wa{7,xr{{copy it{19306
{{bze{7,xr{6,cmp21{{jump if no success goto{19307
{{zer{13,cmsoc(xs){{{clear success offset fillin ptr{19308
{{bhi{7,xr{3,state{6,cmp20{jump if complex goto{19309
*      here for simple success goto (label)
{{add{8,wa{19,*vrtra{{point to vrtra field as required{19313
{{jsr{6,cdwrd{{{generate success goto{19314
{{brn{6,cmp22{{{jump to deal with fgoto{19315
*      here for complex success goto
{cmp20{beq{7,xr{13,cmfgo(xs){6,cmp22{no code if same as fgoto{19319
{{zer{8,wb{{{else set ok value for cdgvl in wb{19320
{{jsr{6,cdgcg{{{generate code for success goto{19321
{{brn{6,cmp22{{{jump to deal with fgoto{19322
*      here for no success goto
{cmp21{mov{13,cmsoc(xs){3,cwcof{{set success fill in offset{19326
{{mov{8,wa{21,=ocer_{{point to compile error call{19327
{{jsr{6,cdwrd{{{generate as temporary value{19328
{{ejc{{{{{19329
*      cmpil (continued)
*      here to deal with failure goto
{cmp22{mov{7,xr{13,cmfgo(xs){{load failure goto pointer{19335
{{mov{8,wa{7,xr{{copy it{19336
{{zer{13,cmffc(xs){{{set no fill in required yet{19337
{{bze{7,xr{6,cmp23{{jump if no failure goto given{19338
{{add{8,wa{19,*vrtra{{point to vrtra field in case{19339
{{blo{7,xr{3,state{6,cmpse{jump to gen if simple fgoto{19340
*      here for complex failure goto
{{mov{8,wb{3,cwcof{{save offset to o_gof call{19344
{{mov{8,wa{21,=ogof_{{point to failure goto call{19345
{{jsr{6,cdwrd{{{generate{19346
{{mov{8,wa{21,=ofif_{{point to fail in fail word{19347
{{jsr{6,cdwrd{{{generate{19348
{{jsr{6,cdgcg{{{generate code for failure goto{19349
{{mov{8,wa{8,wb{{copy offset to o_gof for cdfal{19350
{{mov{8,wb{22,=b_cdc{{set complex case cdtyp{19351
{{brn{6,cmp25{{{jump to build cdblk{19352
*      here if no failure goto given
{cmp23{mov{8,wa{21,=ounf_{{load unexpected failure call in cas{19356
{{mov{8,wc{3,cswfl{{get -nofail flag{19357
{{orb{8,wc{13,cmcgo(xs){{check if conditional goto{19358
{{zrb{8,wc{6,cmpse{{jump if -nofail and no cond. goto{19359
{{mnz{13,cmffc(xs){{{else set fill in flag{19360
{{mov{8,wa{21,=ocer_{{and set compile error for temporary{19361
*      merge here with cdfal value in wa, simple cdblk
*      also special entry after statement error
{cmpse{mov{8,wb{22,=b_cds{{set cdtyp for simple case{19366
{{ejc{{{{{19367
*      cmpil (continued)
*      merge here to build cdblk
*      (wa)                  cdfal value to be generated
*      (wb)                  cdtyp value to be generated
*      at this stage, we chop off an appropriate chunk of the
*      current ccblk and convert it into a cdblk. the remainder
*      of the ccblk is reformatted to be the new ccblk.
{cmp25{mov{7,xr{3,r_ccb{{point to ccblk{19380
{{mov{7,xl{13,cmlbl(xs){{get possible label pointer{19381
{{bze{7,xl{6,cmp26{{skip if no label{19382
{{zer{13,cmlbl(xs){{{clear flag for next statement{19383
{{mov{13,vrlbl(xl){7,xr{{put cdblk ptr in vrblk label field{19384
*      merge after doing label
{cmp26{mov{9,(xr){8,wb{{set type word for new cdblk{19388
{{mov{13,cdfal(xr){8,wa{{set failure word{19389
{{mov{7,xl{7,xr{{copy pointer to ccblk{19390
{{mov{8,wb{13,ccuse(xr){{load length gen (= new cdlen){19391
{{mov{8,wc{13,cclen(xr){{load total ccblk length{19392
{{add{7,xl{8,wb{{point past cdblk{19393
{{sub{8,wc{8,wb{{get length left for chop off{19394
{{mov{9,(xl){22,=b_cct{{set type code for new ccblk at end{19395
{{mov{13,ccuse(xl){19,*cccod{{set initial code offset{19396
{{mov{3,cwcof{19,*cccod{{reinitialise cwcof{19397
{{mov{13,cclen(xl){8,wc{{set new length{19398
{{mov{3,r_ccb{7,xl{{set new ccblk pointer{19399
{{zer{13,ccsln(xl){{{initialize new line number{19401
{{mov{13,cdsln(xr){3,cmpln{{set line number in old block{19402
{{mov{13,cdstm(xr){3,cmpsn{{set statement number{19404
{{icv{3,cmpsn{{{bump statement number{19405
*      set pointers in previous code block as required
{{mov{7,xl{13,cmpcd(xs){{load ptr to previous cdblk{19409
{{bze{13,cmffp(xs){6,cmp27{{jump if no failure fill in required{19410
{{mov{13,cdfal(xl){7,xr{{else set failure ptr in previous{19411
*      here to deal with success forward pointer
{cmp27{mov{8,wa{13,cmsop(xs){{load success offset{19415
{{bze{8,wa{6,cmp28{{jump if no fill in required{19416
{{add{7,xl{8,wa{{else point to fill in location{19417
{{mov{9,(xl){7,xr{{store forward pointer{19418
{{zer{7,xl{{{clear garbage xl value{19419
{{ejc{{{{{19420
*      cmpil (continued)
*      now set fill in pointers for this statement
{cmp28{mov{13,cmffp(xs){13,cmffc(xs){{copy failure fill in flag{19426
{{mov{13,cmsop(xs){13,cmsoc(xs){{copy success fill in offset{19427
{{mov{13,cmpcd(xs){7,xr{{save ptr to this cdblk{19428
{{bnz{13,cmtra(xs){6,cmp29{{jump if initial entry already set{19429
{{mov{13,cmtra(xs){7,xr{{else set ptr here as default{19430
*      here after compiling one statement
{cmp29{blt{3,stage{18,=stgce{6,cmp01{jump if not end line just done{19434
{{bze{3,cswls{6,cmp30{{skip if -nolist{19435
{{jsr{6,listr{{{list last line{19436
*      return
{cmp30{mov{7,xr{13,cmtra(xs){{load initial entry cdblk pointer{19440
{{add{7,xs{19,*cmnen{{pop work locations off stack{19441
{{exi{{{{and return to cmpil caller{19442
*      here at end of goto field
{cmp31{mov{8,wb{13,cmfgo(xs){{get fail goto{19446
{{orb{8,wb{13,cmsgo(xs){{or in success goto{19447
{{bnz{8,wb{6,cmp18{{ok if non-null field{19448
{{erb{1,219{26,syntax error: empty goto field{{{19449
*      control card found
{cmp32{icv{8,wb{{{point past ch_mn{19453
{{jsr{6,cncrd{{{process control card{19454
{{zer{3,scnse{{{clear start of element loc.{19455
{{brn{6,cmpce{{{loop for next statement{19456
{{enp{{{{end procedure cmpil{19457
{{ejc{{{{{19458
*      cncrd -- control card processor
*      called to deal with control cards
*      r_cim                 points to current image
*      (wb)                  offset to 1st char of control card
*      jsr  cncrd            call to process control cards
*      (xl,xr,wa,wb,wc,ia)   destroyed
{cncrd{prc{25,e{1,0{{entry point{19469
{{mov{3,scnpt{8,wb{{offset for control card scan{19470
{{mov{8,wa{18,=ccnoc{{number of chars for comparison{19471
{{ctw{8,wa{1,0{{convert to word count{19472
{{mov{3,cnswc{8,wa{{save word count{19473
*      loop here if more than one control card
{cnc01{bge{3,scnpt{3,scnil{6,cnc09{return if end of image{19477
{{mov{7,xr{3,r_cim{{point to image{19478
{{plc{7,xr{3,scnpt{{char ptr for first char{19479
{{lch{8,wa{10,(xr)+{{get first char{19480
{{beq{8,wa{18,=ch_li{6,cnc07{special case of -inxxx{19484
{cnc0a{mnz{3,scncc{{{set flag for scane{19485
{{jsr{6,scane{{{scan card name{19486
{{zer{3,scncc{{{clear scane flag{19487
{{bnz{7,xl{6,cnc06{{fail unless control card name{19488
{{mov{8,wa{18,=ccnoc{{no. of chars to be compared{19489
{{blt{13,sclen(xr){8,wa{6,cnc08{fail if too few chars{19491
{{mov{7,xl{7,xr{{point to control card name{19495
{{zer{8,wb{{{zero offset for substring{19496
{{jsr{6,sbstr{{{extract substring for comparison{19497
{{mov{3,cnscc{7,xr{{keep control card substring ptr{19502
{{mov{7,xr{21,=ccnms{{point to list of standard names{19503
{{zer{8,wb{{{initialise name offset{19504
{{lct{8,wc{18,=cc_nc{{number of standard names{19505
*      try to match name
{cnc02{mov{7,xl{3,cnscc{{point to name{19509
{{lct{8,wa{3,cnswc{{counter for inner loop{19510
{{brn{6,cnc04{{{jump into loop{19511
*      inner loop to match card name chars
{cnc03{ica{7,xr{{{bump standard names ptr{19515
{{ica{7,xl{{{bump name pointer{19516
*      here to initiate the loop
{cnc04{cne{13,schar(xl){9,(xr){6,cnc05{comp. up to cfp_c chars at once{19520
{{bct{8,wa{6,cnc03{{loop if more words to compare{19521
{{ejc{{{{{19522
*      cncrd (continued)
*      matched - branch on card offset
{{mov{7,xl{8,wb{{get name offset{19528
{{bsw{7,xl{2,cc_nc{6,cnc08{switch{19530
{{iff{2,cc_do{6,cnc10{{-double{19569
{{iff{1,1{6,cnc08{{{19569
{{iff{2,cc_du{6,cnc11{{-dump{19569
{{iff{2,cc_cp{6,cnc41{{-copy{19569
{{iff{2,cc_ej{6,cnc12{{-eject{19569
{{iff{2,cc_er{6,cnc13{{-errors{19569
{{iff{2,cc_ex{6,cnc14{{-execute{19569
{{iff{2,cc_fa{6,cnc15{{-fail{19569
{{iff{2,cc_in{6,cnc41{{-include{19569
{{iff{2,cc_ln{6,cnc44{{-line{19569
{{iff{2,cc_li{6,cnc16{{-list{19569
{{iff{2,cc_nr{6,cnc17{{-noerrors{19569
{{iff{2,cc_nx{6,cnc18{{-noexecute{19569
{{iff{2,cc_nf{6,cnc19{{-nofail{19569
{{iff{2,cc_nl{6,cnc20{{-nolist{19569
{{iff{2,cc_no{6,cnc21{{-noopt{19569
{{iff{2,cc_np{6,cnc22{{-noprint{19569
{{iff{2,cc_op{6,cnc24{{-optimise{19569
{{iff{2,cc_pr{6,cnc25{{-print{19569
{{iff{2,cc_si{6,cnc27{{-single{19569
{{iff{2,cc_sp{6,cnc28{{-space{19569
{{iff{2,cc_st{6,cnc31{{-stitle{19569
{{iff{2,cc_ti{6,cnc32{{-title{19569
{{iff{2,cc_tr{6,cnc36{{-trace{19569
{{esw{{{{end switch{19569
*      not matched yet. align std names ptr and try again
{cnc05{ica{7,xr{{{bump standard names ptr{19573
{{bct{8,wa{6,cnc05{{loop{19574
{{icv{8,wb{{{bump names offset{19575
{{bct{8,wc{6,cnc02{{continue if more names{19576
{{brn{6,cnc08{{{ignore unrecognized control card{19578
*      invalid control card name
{cnc06{erb{1,247{26,invalid control statement{{{19583
*      special processing for -inxxx
{cnc07{lch{8,wa{10,(xr)+{{get next char{19587
{{bne{8,wa{18,=ch_ln{6,cnc0a{if not letter n{19591
{{lch{8,wa{9,(xr){{get third char{19592
{{blt{8,wa{18,=ch_d0{6,cnc0a{if not digit{19593
{{bgt{8,wa{18,=ch_d9{6,cnc0a{if not digit{19594
{{add{3,scnpt{18,=num02{{bump offset past -in{19595
{{jsr{6,scane{{{scan integer after -in{19596
{{mov{11,-(xs){7,xr{{stack scanned item{19597
{{jsr{6,gtsmi{{{check if integer{19598
{{ppm{6,cnc06{{{fail if not integer{19599
{{ppm{6,cnc06{{{fail if negative or large{19600
{{mov{3,cswin{7,xr{{keep integer{19601
{{ejc{{{{{19602
*      cncrd (continued)
*      check for more control cards before returning
{cnc08{mov{8,wa{3,scnpt{{preserve in case xeq time compile{19608
{{jsr{6,scane{{{look for comma{19609
{{beq{7,xl{18,=t_cma{6,cnc01{loop if comma found{19610
{{mov{3,scnpt{8,wa{{restore scnpt in case xeq time{19611
*      return point
{cnc09{exi{{{{return{19615
*      -double
{cnc10{mnz{3,cswdb{{{set switch{19619
{{brn{6,cnc08{{{merge{19620
*      -dump
*      this is used for system debugging . it has the effect of
*      producing a core dump at compilation time
{cnc11{jsr{6,sysdm{{{call dumper{19626
{{brn{6,cnc09{{{finished{19627
*      -eject
{cnc12{bze{3,cswls{6,cnc09{{return if -nolist{19631
{{jsr{6,prtps{{{eject{19632
{{jsr{6,listt{{{list title{19633
{{brn{6,cnc09{{{finished{19634
*      -errors
{cnc13{zer{3,cswer{{{clear switch{19638
{{brn{6,cnc08{{{merge{19639
*      -execute
{cnc14{zer{3,cswex{{{clear switch{19643
{{brn{6,cnc08{{{merge{19644
*      -fail
{cnc15{mnz{3,cswfl{{{set switch{19648
{{brn{6,cnc08{{{merge{19649
*      -list
{cnc16{mnz{3,cswls{{{set switch{19653
{{beq{3,stage{18,=stgic{6,cnc08{done if compile time{19654
*      list code line if execute time compile
{{zer{3,lstpf{{{permit listing{19658
{{jsr{6,listr{{{list line{19659
{{brn{6,cnc08{{{merge{19660
{{ejc{{{{{19661
*      cncrd (continued)
*      -noerrors
{cnc17{mnz{3,cswer{{{set switch{19667
{{brn{6,cnc08{{{merge{19668
*      -noexecute
{cnc18{mnz{3,cswex{{{set switch{19672
{{brn{6,cnc08{{{merge{19673
*      -nofail
{cnc19{zer{3,cswfl{{{clear switch{19677
{{brn{6,cnc08{{{merge{19678
*      -nolist
{cnc20{zer{3,cswls{{{clear switch{19682
{{brn{6,cnc08{{{merge{19683
*      -nooptimise
{cnc21{mnz{3,cswno{{{set switch{19687
{{brn{6,cnc08{{{merge{19688
*      -noprint
{cnc22{zer{3,cswpr{{{clear switch{19692
{{brn{6,cnc08{{{merge{19693
*      -optimise
{cnc24{zer{3,cswno{{{clear switch{19697
{{brn{6,cnc08{{{merge{19698
*      -print
{cnc25{mnz{3,cswpr{{{set switch{19702
{{brn{6,cnc08{{{merge{19703
{{ejc{{{{{19704
*      cncrd (continued)
*      -single
{cnc27{zer{3,cswdb{{{clear switch{19710
{{brn{6,cnc08{{{merge{19711
*      -space
{cnc28{bze{3,cswls{6,cnc09{{return if -nolist{19715
{{jsr{6,scane{{{scan integer after -space{19716
{{mov{8,wc{18,=num01{{1 space in case{19717
{{beq{7,xr{18,=t_smc{6,cnc29{jump if no integer{19718
{{mov{11,-(xs){7,xr{{stack it{19719
{{jsr{6,gtsmi{{{check integer{19720
{{ppm{6,cnc06{{{fail if not integer{19721
{{ppm{6,cnc06{{{fail if negative or large{19722
{{bnz{8,wc{6,cnc29{{jump if non zero{19723
{{mov{8,wc{18,=num01{{else 1 space{19724
*      merge with count of lines to skip
{cnc29{add{3,lstlc{8,wc{{bump line count{19728
{{lct{8,wc{8,wc{{convert to loop counter{19729
{{blt{3,lstlc{3,lstnp{6,cnc30{jump if fits on page{19730
{{jsr{6,prtps{{{eject{19731
{{jsr{6,listt{{{list title{19732
{{brn{6,cnc09{{{merge{19733
*      skip lines
{cnc30{jsr{6,prtnl{{{print a blank{19737
{{bct{8,wc{6,cnc30{{loop{19738
{{brn{6,cnc09{{{merge{19739
{{ejc{{{{{19740
*      cncrd (continued)
*      -stitl
{cnc31{mov{3,cnr_t{20,=r_stl{{ptr to r_stl{19746
{{brn{6,cnc33{{{merge{19747
*      -title
{cnc32{mov{3,r_stl{21,=nulls{{clear subtitle{19751
{{mov{3,cnr_t{20,=r_ttl{{ptr to r_ttl{19752
*      common processing for -title, -stitl
{cnc33{mov{7,xr{21,=nulls{{null in case needed{19756
{{mnz{3,cnttl{{{set flag for next listr call{19757
{{mov{8,wb{18,=ccofs{{offset to title/subtitle{19758
{{mov{8,wa{3,scnil{{input image length{19759
{{blo{8,wa{8,wb{6,cnc34{jump if no chars left{19760
{{sub{8,wa{8,wb{{no of chars to extract{19761
{{mov{7,xl{3,r_cim{{point to image{19762
{{jsr{6,sbstr{{{get title/subtitle{19763
*      store title/subtitle
{cnc34{mov{7,xl{3,cnr_t{{point to storage location{19767
{{mov{9,(xl){7,xr{{store title/subtitle{19768
{{beq{7,xl{20,=r_stl{6,cnc09{return if stitl{19769
{{bnz{3,precl{6,cnc09{{return if extended listing{19770
{{bze{3,prich{6,cnc09{{return if regular printer{19771
{{mov{7,xl{13,sclen(xr){{get length of title{19772
{{mov{8,wa{7,xl{{copy it{19773
{{bze{7,xl{6,cnc35{{jump if null{19774
{{add{7,xl{18,=num10{{increment{19775
{{bhi{7,xl{3,prlen{6,cnc09{use default lstp0 val if too long{19776
{{add{8,wa{18,=num04{{point just past title{19777
*      store offset to page nn message for short title
{cnc35{mov{3,lstpo{8,wa{{store offset{19781
{{brn{6,cnc09{{{return{19782
*      -trace
*      provided for system debugging.  toggles the system label
*      trace switch at compile time
{cnc36{jsr{6,systt{{{toggle switch{19788
{{brn{6,cnc08{{{merge{19789
*      -include
{cnc41{mnz{3,scncc{{{set flag for scane{19827
{{jsr{6,scane{{{scan quoted file name{19828
{{zer{3,scncc{{{clear scane flag{19829
{{bne{7,xl{18,=t_con{6,cnc06{if not constant{19830
{{bne{9,(xr){22,=b_scl{6,cnc06{if not string constant{19831
{{mov{3,r_ifn{7,xr{{save file name{19832
{{mov{7,xl{3,r_inc{{examine include file name table{19833
{{zer{8,wb{{{lookup by value{19834
{{jsr{6,tfind{{{do lookup{19835
{{ppm{{{{never fails{19836
{{beq{7,xr{21,=inton{6,cnc09{ignore if already in table{19837
{{mnz{8,wb{{{set for trim{19838
{{mov{7,xr{3,r_ifn{{file name{19839
{{jsr{6,trimr{{{remove trailing blanks{19840
{{mov{7,xl{3,r_inc{{include file name table{19841
{{mnz{8,wb{{{lookup by name this time{19842
{{jsr{6,tfind{{{do lookup{19843
{{ppm{{{{never fails{19844
{{mov{13,teval(xl){21,=inton{{make table value integer 1{19845
{{icv{3,cnind{{{increase nesting level{19846
{{mov{8,wa{3,cnind{{load new nest level{19847
{{bgt{8,wa{18,=ccinm{6,cnc42{fail if excessive nesting{19848
*      record the name and line number of the current input file
{{mov{7,xl{3,r_ifa{{array of nested file names{19853
{{add{8,wa{18,=vcvlb{{compute offset in words{19854
{{wtb{8,wa{{{convert to bytes{19855
{{add{7,xl{8,wa{{point to element{19856
{{mov{9,(xl){3,r_sfc{{record current file name{19857
{{mov{7,xl{8,wa{{preserve nesting byte offset{19858
{{mti{3,rdnln{{{fetch source line number as integer{19859
{{jsr{6,icbld{{{convert to icblk{19860
{{add{7,xl{3,r_ifl{{entry in nested line number array{19861
{{mov{9,(xl){7,xr{{record in array{19862
*      here to switch to include file named in r_ifn
{{mov{8,wa{3,cswin{{max read length{19867
{{mov{7,xl{3,r_ifn{{include file name{19868
{{jsr{6,alocs{{{get buffer for complete file name{19869
{{jsr{6,sysif{{{open include file{19870
{{ppm{6,cnc43{{{could not open{19871
*      make note of the complete file name for error messages
{{zer{8,wb{{{do not trim trailing blanks{19876
{{jsr{6,trimr{{{adjust scblk for actual length{19877
{{mov{3,r_sfc{7,xr{{save ptr to file name{19878
{{mti{3,cmpsn{{{current statement as integer{19879
{{jsr{6,icbld{{{build icblk for stmt number{19880
{{mov{7,xl{3,r_sfn{{file name table{19881
{{mnz{8,wb{{{lookup statement number by name{19882
{{jsr{6,tfind{{{allocate new teblk{19883
{{ppm{{{{always possible to allocate block{19884
{{mov{13,teval(xl){3,r_sfc{{record file name as entry value{19885
{{zer{3,rdnln{{{restart line counter for new file{19889
{{beq{3,stage{18,=stgic{6,cnc09{if initial compile{19890
{{bne{3,cnind{18,=num01{6,cnc09{if not first execute-time nesting{19891
*      here for -include during execute-time compile
{{mov{3,r_ici{3,r_cim{{remember code argument string{19895
{{mov{3,cnspt{3,scnpt{{save position in string{19896
{{mov{3,cnsil{3,scnil{{and length of string{19897
{{brn{6,cnc09{{{all done, merge{19898
*      here for excessive include file nesting
{cnc42{erb{1,284{26,excessively nested include files{{{19902
*      here if include file could not be opened
{cnc43{mov{3,dnamp{7,xr{{release allocated scblk{19906
{{erb{1,285{26,include file cannot be opened{{{19907
*      -line n filename
{cnc44{jsr{6,scane{{{scan integer after -line{19914
{{bne{7,xl{18,=t_con{6,cnc06{jump if no line number{19915
{{bne{9,(xr){22,=b_icl{6,cnc06{jump if not integer{19916
{{ldi{13,icval(xr){{{fetch integer line number{19917
{{ile{6,cnc06{{{error if negative or zero{19918
{{beq{3,stage{18,=stgic{6,cnc45{skip if initial compile{19919
{{mfi{3,cmpln{{{set directly for other compiles{19920
{{brn{6,cnc46{{{no need to set rdnln{19921
{cnc45{sbi{4,intv1{{{adjust number by one{19922
{{mfi{3,rdnln{{{save line number{19923
{cnc46{mnz{3,scncc{{{set flag for scane{19925
{{jsr{6,scane{{{scan quoted file name{19926
{{zer{3,scncc{{{clear scane flag{19927
{{beq{7,xl{18,=t_smc{6,cnc47{done if no file name{19928
{{bne{7,xl{18,=t_con{6,cnc06{error if not constant{19929
{{bne{9,(xr){22,=b_scl{6,cnc06{if not string constant{19930
{{jsr{6,newfn{{{record new file name{19931
{{brn{6,cnc09{{{merge{19932
*      here if file name not present
{cnc47{dcv{3,scnpt{{{set to rescan the terminator{19936
{{brn{6,cnc09{{{merge{19937
{{enp{{{{end procedure cncrd{19942
{{ejc{{{{{19943
*      dffnc -- define function
*      dffnc is called whenever a new function is assigned to
*      a variable. it deals with external function use counts.
*      (xr)                  pointer to vrblk
*      (xl)                  pointer to new function block
*      jsr  dffnc            call to define function
*      (wa,wb)               destroyed
{dffnc{prc{25,e{1,0{{entry point{20025
{{bne{9,(xl){22,=b_efc{6,dffn1{skip if new function not external{20028
{{icv{13,efuse(xl){{{else increment its use count{20029
*      here after dealing with new function use count
{dffn1{mov{8,wa{7,xr{{save vrblk pointer{20033
{{mov{7,xr{13,vrfnc(xr){{load old function pointer{20034
{{bne{9,(xr){22,=b_efc{6,dffn2{jump if old function not external{20035
{{mov{8,wb{13,efuse(xr){{else get use count{20036
{{dcv{8,wb{{{decrement{20037
{{mov{13,efuse(xr){8,wb{{store decremented value{20038
{{bnz{8,wb{6,dffn2{{jump if use count still non-zero{20039
{{jsr{6,sysul{{{else call system unload function{20040
*      here after dealing with old function use count
{dffn2{mov{7,xr{8,wa{{restore vrblk pointer{20044
{{mov{8,wa{7,xl{{copy function block ptr{20046
{{blt{7,xr{20,=r_yyy{6,dffn3{skip checks if opsyn op definition{20047
{{bnz{13,vrlen(xr){6,dffn3{{jump if not system variable{20048
*      for system variable, check for illegal redefinition
{{mov{7,xl{13,vrsvp(xr){{point to svblk{20052
{{mov{8,wb{13,svbit(xl){{load bit indicators{20053
{{anb{8,wb{4,btfnc{{is it a system function{20054
{{zrb{8,wb{6,dffn3{{redef ok if not{20055
{{erb{1,248{26,attempted redefinition of system function{{{20056
*      here if redefinition is permitted
{dffn3{mov{13,vrfnc(xr){8,wa{{store new function pointer{20060
{{mov{7,xl{8,wa{{restore function block pointer{20061
{{exi{{{{return to dffnc caller{20062
{{enp{{{{end procedure dffnc{20063
{{ejc{{{{{20064
*      dtach -- detach i/o associated names
*      detaches trblks from i/o associated variables, removes
*      entry from iochn chain attached to filearg1 vrblk and may
*      remove vrblk access and store traps.
*      input, output, terminal are handled specially.
*      (xl)                  i/o assoc. vbl name base ptr
*      (wa)                  offset to name
*      jsr  dtach            call for detach operation
*      (xl,xr,wa,wb,wc)      destroyed
{dtach{prc{25,e{1,0{{entry point{20078
{{mov{3,dtcnb{7,xl{{store name base (gbcol not called){20079
{{add{7,xl{8,wa{{point to name location{20080
{{mov{3,dtcnm{7,xl{{store it{20081
*      loop to search for i/o trblk
{dtch1{mov{7,xr{7,xl{{copy name pointer{20085
*      continue after block deletion
{dtch2{mov{7,xl{9,(xl){{point to next value{20089
{{bne{9,(xl){22,=b_trt{6,dtch6{jump at chain end{20090
{{mov{8,wa{13,trtyp(xl){{get trap block type{20091
{{beq{8,wa{18,=trtin{6,dtch3{jump if input{20092
{{beq{8,wa{18,=trtou{6,dtch3{jump if output{20093
{{add{7,xl{19,*trnxt{{point to next link{20094
{{brn{6,dtch1{{{loop{20095
*      delete an old association
{dtch3{mov{9,(xr){13,trval(xl){{delete trblk{20099
{{mov{8,wa{7,xl{{dump xl ...{20100
{{mov{8,wb{7,xr{{... and xr{20101
{{mov{7,xl{13,trtrf(xl){{point to trtrf trap block{20102
{{bze{7,xl{6,dtch5{{jump if no iochn{20103
{{bne{9,(xl){22,=b_trt{6,dtch5{jump if input, output, terminal{20104
*      loop to search iochn chain for name ptr
{dtch4{mov{7,xr{7,xl{{remember link ptr{20108
{{mov{7,xl{13,trtrf(xl){{point to next link{20109
{{bze{7,xl{6,dtch5{{jump if end of chain{20110
{{mov{8,wc{13,ionmb(xl){{get name base{20111
{{add{8,wc{13,ionmo(xl){{add offset{20112
{{bne{8,wc{3,dtcnm{6,dtch4{loop if no match{20113
{{mov{13,trtrf(xr){13,trtrf(xl){{remove name from chain{20114
{{ejc{{{{{20115
*      dtach (continued)
*      prepare to resume i/o trblk scan
{dtch5{mov{7,xl{8,wa{{recover xl ...{20121
{{mov{7,xr{8,wb{{... and xr{20122
{{add{7,xl{19,*trval{{point to value field{20123
{{brn{6,dtch2{{{continue{20124
*      exit point
{dtch6{mov{7,xr{3,dtcnb{{possible vrblk ptr{20128
{{jsr{6,setvr{{{reset vrblk if necessary{20129
{{exi{{{{return{20130
{{enp{{{{end procedure dtach{20131
{{ejc{{{{{20132
*      dtype -- get datatype name
*      (xr)                  object whose datatype is required
*      jsr  dtype            call to get datatype
*      (xr)                  result datatype
{dtype{prc{25,e{1,0{{entry point{20140
{{beq{9,(xr){22,=b_pdt{6,dtyp1{jump if prog.defined{20141
{{mov{7,xr{9,(xr){{load type word{20142
{{lei{7,xr{{{get entry point id (block code){20143
{{wtb{7,xr{{{convert to byte offset{20144
{{mov{7,xr{14,scnmt(xr){{load table entry{20145
{{exi{{{{exit to dtype caller{20146
*      here if program defined
{dtyp1{mov{7,xr{13,pddfp(xr){{point to dfblk{20150
{{mov{7,xr{13,dfnam(xr){{get datatype name from dfblk{20151
{{exi{{{{return to dtype caller{20152
{{enp{{{{end procedure dtype{20153
{{ejc{{{{{20154
*      dumpr -- print dump of storage
*      (xr)                  dump argument (see below)
*      jsr  dumpr            call to print dump
*      (xr,xl)               destroyed
*      (wa,wb,wc,ra)         destroyed
*      the dump argument has the following significance
*      dmarg = 0             no dump printed
*      dmarg = 1             partial dump (nat vars, keywords)
*      dmarg = 2             full dump (arrays, tables, etc.)
*      dmarg = 3             full dump + null variables
*      dmarg ge 4            core dump
*      since dumpr scrambles store, it is not permissible to
*      collect in mid-dump. hence a collect is done initially
*      and then if store runs out an error message is produced.
{dumpr{prc{25,e{1,0{{entry point{20175
{{bze{7,xr{6,dmp28{{skip dump if argument is zero{20176
{{bgt{7,xr{18,=num03{6,dmp29{jump if core dump required{20177
{{zer{7,xl{{{clear xl{20178
{{zer{8,wb{{{zero move offset{20179
{{mov{3,dmarg{7,xr{{save dump argument{20180
{{zer{3,dnams{{{collect sediment too{20182
{{jsr{6,gbcol{{{collect garbage{20184
{{jsr{6,prtpg{{{eject printer{20185
{{mov{7,xr{21,=dmhdv{{point to heading for variables{20186
{{jsr{6,prtst{{{print it{20187
{{jsr{6,prtnl{{{terminate print line{20188
{{jsr{6,prtnl{{{and print a blank line{20189
*      first all natural variable blocks (vrblk) whose values
*      are non-null are linked in lexical order using dmvch as
*      the chain head and chaining through the vrget fields.
*      note that this scrambles store if the process is
*      interrupted before completion e.g. by exceeding time  or
*      print limits. since the subsequent core dumps and
*      failures if execution is resumed are very confusing, the
*      execution time error routine checks for this event and
*      attempts an unscramble. similar precautions should be
*      observed if translate time dumping is implemented.
{{zer{3,dmvch{{{set null chain to start{20202
{{mov{8,wa{3,hshtb{{point to hash table{20203
*      loop through headers in hash table
{dmp00{mov{7,xr{8,wa{{copy hash bucket pointer{20207
{{ica{8,wa{{{bump pointer{20208
{{sub{7,xr{19,*vrnxt{{set offset to merge{20209
*      loop through vrblks on one chain
{dmp01{mov{7,xr{13,vrnxt(xr){{point to next vrblk on chain{20213
{{bze{7,xr{6,dmp09{{jump if end of this hash chain{20214
{{mov{7,xl{7,xr{{else copy vrblk pointer{20215
{{ejc{{{{{20216
*      dumpr (continued)
*      loop to find value and skip if null
{dmp02{mov{7,xl{13,vrval(xl){{load value{20222
{{beq{3,dmarg{18,=num03{6,dmp2a{skip null value check if dump(3){20223
{{beq{7,xl{21,=nulls{6,dmp01{loop for next vrblk if null value{20224
{dmp2a{beq{9,(xl){22,=b_trt{6,dmp02{loop back if value is trapped{20225
*      non-null value, prepare to search chain
{{mov{8,wc{7,xr{{save vrblk pointer{20229
{{add{7,xr{19,*vrsof{{adjust ptr to be like scblk ptr{20230
{{bnz{13,sclen(xr){6,dmp03{{jump if non-system variable{20231
{{mov{7,xr{13,vrsvo(xr){{else load ptr to name in svblk{20232
*      here with name pointer for new block in xr
{dmp03{mov{8,wb{7,xr{{save pointer to chars{20236
{{mov{3,dmpsv{8,wa{{save hash bucket pointer{20237
{{mov{8,wa{20,=dmvch{{point to chain head{20238
*      loop to search chain for correct insertion point
{dmp04{mov{3,dmpch{8,wa{{save chain pointer{20242
{{mov{7,xl{8,wa{{copy it{20243
{{mov{7,xr{9,(xl){{load pointer to next entry{20244
{{bze{7,xr{6,dmp08{{jump if end of chain to insert{20245
{{add{7,xr{19,*vrsof{{else get name ptr for chained vrblk{20246
{{bnz{13,sclen(xr){6,dmp05{{jump if not system variable{20247
{{mov{7,xr{13,vrsvo(xr){{else point to name in svblk{20248
*      here prepare to compare the names
*      (wa)                  scratch
*      (wb)                  pointer to string of entering vrblk
*      (wc)                  pointer to entering vrblk
*      (xr)                  pointer to string of current block
*      (xl)                  scratch
{dmp05{mov{7,xl{8,wb{{point to entering vrblk string{20258
{{mov{8,wa{13,sclen(xl){{load its length{20259
{{plc{7,xl{{{point to chars of entering string{20260
{{bhi{8,wa{13,sclen(xr){6,dmp06{jump if entering length high{20283
{{plc{7,xr{{{else point to chars of old string{20284
{{cmc{6,dmp08{6,dmp07{{compare, insert if new is llt old{20285
{{brn{6,dmp08{{{or if leq (we had shorter length){20286
*      here when new length is longer than old length
{dmp06{mov{8,wa{13,sclen(xr){{load shorter length{20290
{{plc{7,xr{{{point to chars of old string{20291
{{cmc{6,dmp08{6,dmp07{{compare, insert if new one low{20292
{{ejc{{{{{20293
*      dumpr (continued)
*      here we move out on the chain
{dmp07{mov{7,xl{3,dmpch{{copy chain pointer{20299
{{mov{8,wa{9,(xl){{move to next entry on chain{20301
{{brn{6,dmp04{{{loop back{20302
*      here after locating the proper insertion point
{dmp08{mov{7,xl{3,dmpch{{copy chain pointer{20306
{{mov{8,wa{3,dmpsv{{restore hash bucket pointer{20307
{{mov{7,xr{8,wc{{restore vrblk pointer{20308
{{mov{13,vrget(xr){9,(xl){{link vrblk to rest of chain{20309
{{mov{9,(xl){7,xr{{link vrblk into current chain loc{20310
{{brn{6,dmp01{{{loop back for next vrblk{20311
*      here after processing all vrblks on one chain
{dmp09{bne{8,wa{3,hshte{6,dmp00{loop back if more buckets to go{20315
*      loop to generate dump of natural variable values
{dmp10{mov{7,xr{3,dmvch{{load pointer to next entry on chain{20319
{{bze{7,xr{6,dmp11{{jump if end of chain{20320
{{mov{3,dmvch{9,(xr){{else update chain ptr to next entry{20321
{{jsr{6,setvr{{{restore vrget field{20322
{{mov{7,xl{7,xr{{copy vrblk pointer (name base){20323
{{mov{8,wa{19,*vrval{{set offset for vrblk name{20324
{{jsr{6,prtnv{{{print name = value{20325
{{brn{6,dmp10{{{loop back till all printed{20326
*      prepare to print keywords
{dmp11{jsr{6,prtnl{{{print blank line{20330
{{jsr{6,prtnl{{{and another{20331
{{mov{7,xr{21,=dmhdk{{point to keyword heading{20332
{{jsr{6,prtst{{{print heading{20333
{{jsr{6,prtnl{{{end line{20334
{{jsr{6,prtnl{{{print one blank line{20335
{{mov{7,xl{21,=vdmkw{{point to list of keyword svblk ptrs{20336
{{ejc{{{{{20337
*      dumpr (continued)
*      loop to dump keyword values
{dmp12{mov{7,xr{10,(xl)+{{load next svblk ptr from table{20343
{{bze{7,xr{6,dmp13{{jump if end of list{20344
{{beq{7,xr{18,=num01{6,dmp12{&compare ignored if not implemented{20346
{{mov{8,wa{18,=ch_am{{load ampersand{20348
{{jsr{6,prtch{{{print ampersand{20349
{{jsr{6,prtst{{{print keyword name{20350
{{mov{8,wa{13,svlen(xr){{load name length from svblk{20351
{{ctb{8,wa{2,svchs{{get length of name{20352
{{add{7,xr{8,wa{{point to svknm field{20353
{{mov{3,dmpkn{9,(xr){{store in dummy kvblk{20354
{{mov{7,xr{21,=tmbeb{{point to blank-equal-blank{20355
{{jsr{6,prtst{{{print it{20356
{{mov{3,dmpsv{7,xl{{save table pointer{20357
{{mov{7,xl{20,=dmpkb{{point to dummy kvblk{20358
{{mov{9,(xl){22,=b_kvt{{build type word{20359
{{mov{13,kvvar(xl){21,=trbkv{{build ptr to dummy trace block{20360
{{mov{8,wa{19,*kvvar{{set zero offset{20361
{{jsr{6,acess{{{get keyword value{20362
{{ppm{{{{failure is impossible{20363
{{jsr{6,prtvl{{{print keyword value{20364
{{jsr{6,prtnl{{{terminate print line{20365
{{mov{7,xl{3,dmpsv{{restore table pointer{20366
{{brn{6,dmp12{{{loop back till all printed{20367
*      here after completing partial dump
{dmp13{beq{3,dmarg{18,=num01{6,dmp27{exit if partial dump complete{20371
{{mov{7,xr{3,dnamb{{else point to first dynamic block{20372
*      loop through blocks in dynamic storage
{dmp14{beq{7,xr{3,dnamp{6,dmp27{jump if end of used region{20376
{{mov{8,wa{9,(xr){{else load first word of block{20377
{{beq{8,wa{22,=b_vct{6,dmp16{jump if vector{20378
{{beq{8,wa{22,=b_art{6,dmp17{jump if array{20379
{{beq{8,wa{22,=b_pdt{6,dmp18{jump if program defined{20380
{{beq{8,wa{22,=b_tbt{6,dmp19{jump if table{20381
*      merge here to move to next block
{dmp15{jsr{6,blkln{{{get length of block{20389
{{add{7,xr{8,wa{{point past this block{20390
{{brn{6,dmp14{{{loop back for next block{20391
{{ejc{{{{{20392
*      dumpr (continued)
*      here for vector
{dmp16{mov{8,wb{19,*vcvls{{set offset to first value{20398
{{brn{6,dmp19{{{jump to merge{20399
*      here for array
{dmp17{mov{8,wb{13,arofs(xr){{set offset to arpro field{20403
{{ica{8,wb{{{bump to get offset to values{20404
{{brn{6,dmp19{{{jump to merge{20405
*      here for program defined
{dmp18{mov{8,wb{19,*pdfld{{point to values, merge{20409
*      here for table (others merge)
{dmp19{bze{13,idval(xr){6,dmp15{{ignore block if zero id value{20413
{{jsr{6,blkln{{{else get block length{20414
{{mov{7,xl{7,xr{{copy block pointer{20415
{{mov{3,dmpsv{8,wa{{save length{20416
{{mov{8,wa{8,wb{{copy offset to first value{20417
{{jsr{6,prtnl{{{print blank line{20418
{{mov{3,dmpsa{8,wa{{preserve offset{20419
{{jsr{6,prtvl{{{print block value (for title){20420
{{mov{8,wa{3,dmpsa{{recover offset{20421
{{jsr{6,prtnl{{{end print line{20422
{{beq{9,(xr){22,=b_tbt{6,dmp22{jump if table{20423
{{dca{8,wa{{{point before first word{20424
*      loop to print contents of array, vector, or program def
{dmp20{mov{7,xr{7,xl{{copy block pointer{20428
{{ica{8,wa{{{bump offset{20429
{{add{7,xr{8,wa{{point to next value{20430
{{beq{8,wa{3,dmpsv{6,dmp14{exit if end (xr past block){20431
{{sub{7,xr{19,*vrval{{subtract offset to merge into loop{20432
*      loop to find value and ignore nulls
{dmp21{mov{7,xr{13,vrval(xr){{load next value{20436
{{beq{3,dmarg{18,=num03{6,dmp2b{skip null value check if dump(3){20437
{{beq{7,xr{21,=nulls{6,dmp20{loop back if null value{20438
{dmp2b{beq{9,(xr){22,=b_trt{6,dmp21{loop back if trapped{20439
{{jsr{6,prtnv{{{else print name = value{20440
{{brn{6,dmp20{{{loop back for next field{20441
{{ejc{{{{{20442
*      dumpr (continued)
*      here to dump a table
{dmp22{mov{8,wc{19,*tbbuk{{set offset to first bucket{20448
{{mov{8,wa{19,*teval{{set name offset for all teblks{20449
*      loop through table buckets
{dmp23{mov{11,-(xs){7,xl{{save tbblk pointer{20453
{{add{7,xl{8,wc{{point to next bucket header{20454
{{ica{8,wc{{{bump bucket offset{20455
{{sub{7,xl{19,*tenxt{{subtract offset to merge into loop{20456
*      loop to process teblks on one chain
{dmp24{mov{7,xl{13,tenxt(xl){{point to next teblk{20460
{{beq{7,xl{9,(xs){6,dmp26{jump if end of chain{20461
{{mov{7,xr{7,xl{{else copy teblk pointer{20462
*      loop to find value and ignore if null
{dmp25{mov{7,xr{13,teval(xr){{load next value{20466
{{beq{7,xr{21,=nulls{6,dmp24{ignore if null value{20467
{{beq{9,(xr){22,=b_trt{6,dmp25{loop back if trapped{20468
{{mov{3,dmpsv{8,wc{{else save offset pointer{20469
{{jsr{6,prtnv{{{print name = value{20470
{{mov{8,wc{3,dmpsv{{reload offset{20471
{{brn{6,dmp24{{{loop back for next teblk{20472
*      here to move to next hash chain
{dmp26{mov{7,xl{10,(xs)+{{restore tbblk pointer{20476
{{bne{8,wc{13,tblen(xl){6,dmp23{loop back if more buckets to go{20477
{{mov{7,xr{7,xl{{else copy table pointer{20478
{{add{7,xr{8,wc{{point to following block{20479
{{brn{6,dmp14{{{loop back to process next block{20480
*      here after completing dump
{dmp27{jsr{6,prtpg{{{eject printer{20484
*      merge here if no dump given (dmarg=0)
{dmp28{exi{{{{return to dump caller{20488
*      call system core dump routine
{dmp29{jsr{6,sysdm{{{call it{20492
{{brn{6,dmp28{{{return{20493
{{enp{{{{end procedure dumpr{20529
{{ejc{{{{{20530
*      ermsg -- print error code and error message
*      kvert                 error code
*      jsr  ermsg            call to print message
*      (xr,xl,wa,wb,wc,ia)   destroyed
{ermsg{prc{25,e{1,0{{entry point{20538
{{mov{8,wa{3,kvert{{load error code{20539
{{mov{7,xr{21,=ermms{{point to error message /error/{20540
{{jsr{6,prtst{{{print it{20541
{{jsr{6,ertex{{{get error message text{20542
{{add{8,wa{18,=thsnd{{bump error code for print{20543
{{mti{8,wa{{{fail code in int acc{20544
{{mov{8,wb{3,profs{{save current buffer position{20545
{{jsr{6,prtin{{{print code (now have error1xxx){20546
{{mov{7,xl{3,prbuf{{point to print buffer{20547
{{psc{7,xl{8,wb{{point to the 1{20548
{{mov{8,wa{18,=ch_bl{{load a blank{20549
{{sch{8,wa{9,(xl){{store blank over 1 (error xxx){20550
{{csc{7,xl{{{complete store characters{20551
{{zer{7,xl{{{clear garbage pointer in xl{20552
{{mov{8,wa{7,xr{{keep error text{20553
{{mov{7,xr{21,=ermns{{point to / -- /{20554
{{jsr{6,prtst{{{print it{20555
{{mov{7,xr{8,wa{{get error text again{20556
{{jsr{6,prtst{{{print error message text{20557
{{jsr{6,prtis{{{print line{20558
{{jsr{6,prtis{{{print blank line{20559
{{exi{{{{return to ermsg caller{20560
{{enp{{{{end procedure ermsg{20561
{{ejc{{{{{20562
*      ertex -- get error message text
*      (wa)                  error code
*      jsr  ertex            call to get error text
*      (xr)                  ptr to error text in dynamic
*      (r_etx)               copy of ptr to error text
*      (xl,wc,ia)            destroyed
{ertex{prc{25,e{1,0{{entry point{20572
{{mov{3,ertwa{8,wa{{save wa{20573
{{mov{3,ertwb{8,wb{{save wb{20574
{{jsr{6,sysem{{{get failure message text{20575
{{mov{7,xl{7,xr{{copy pointer to it{20576
{{mov{8,wa{13,sclen(xr){{get length of string{20577
{{bze{8,wa{6,ert02{{jump if null{20578
{{zer{8,wb{{{offset of zero{20579
{{jsr{6,sbstr{{{copy into dynamic store{20580
{{mov{3,r_etx{7,xr{{store for relocation{20581
*      return
{ert01{mov{8,wb{3,ertwb{{restore wb{20585
{{mov{8,wa{3,ertwa{{restore wa{20586
{{exi{{{{return to caller{20587
*      return errtext contents instead of null
{ert02{mov{7,xr{3,r_etx{{get errtext{20591
{{brn{6,ert01{{{return{20592
{{enp{{{{{20593
{{ejc{{{{{20594
*      evali -- evaluate integer argument
*      evali is used by pattern primitives len,tab,rtab,pos,rpos
*      when their argument is an expression value.
*      (xr)                  node pointer
*      (wb)                  cursor
*      jsr  evali            call to evaluate integer
*      ppm  loc              transfer loc for non-integer arg
*      ppm  loc              transfer loc for out of range arg
*      ppm  loc              transfer loc for evaluation failure
*      ppm  loc              transfer loc for successful eval
*      (the normal return is never taken)
*      (xr)                  ptr to node with integer argument
*      (wc,xl,ra)            destroyed
*      on return, the node pointed to has the integer argument
*      in parm1 and the proper successor pointer in pthen.
*      this allows merging with the normal (integer arg) case.
{evali{prc{25,r{1,4{{entry point (recursive){20616
{{jsr{6,evalp{{{evaluate expression{20617
{{ppm{6,evli1{{{jump on failure{20618
{{mov{11,-(xs){7,xl{{stack result for gtsmi{20619
{{mov{7,xl{13,pthen(xr){{load successor pointer{20620
{{mov{3,evlio{7,xr{{save original node pointer{20621
{{mov{3,evlif{8,wc{{zero if simple argument{20622
{{jsr{6,gtsmi{{{convert arg to small integer{20623
{{ppm{6,evli2{{{jump if not integer{20624
{{ppm{6,evli3{{{jump if out of range{20625
{{mov{3,evliv{7,xr{{store result in special dummy node{20626
{{mov{7,xr{20,=evlin{{point to dummy node with result{20627
{{mov{9,(xr){22,=p_len{{dummy pattern block pcode{20628
{{mov{13,pthen(xr){7,xl{{store successor pointer{20629
{{exi{1,4{{{take successful exit{20630
*      here if evaluation fails
{evli1{exi{1,3{{{take failure return{20634
*      here if argument is not integer
{evli2{exi{1,1{{{take non-integer error exit{20638
*      here if argument is out of range
{evli3{exi{1,2{{{take out-of-range error exit{20642
{{enp{{{{end procedure evali{20643
{{ejc{{{{{20644
*      evalp -- evaluate expression during pattern match
*      evalp is used to evaluate an expression (by value) during
*      a pattern match. the effect is like evalx, but pattern
*      variables are stacked and restored if necessary.
*      evalp also differs from evalx in that if the result is
*      an expression it is reevaluated. this occurs repeatedly.
*      to support optimization of pos and rpos, evalp uses wc
*      to signal the caller for the case of a simple vrblk
*      that is not an expression and is not trapped.  because
*      this case cannot have any side effects, optimization is
*      possible.
*      (xr)                  node pointer
*      (wb)                  pattern match cursor
*      jsr  evalp            call to evaluate expression
*      ppm  loc              transfer loc if evaluation fails
*      (xl)                  result
*      (wa)                  first word of result block
*      (wc)                  zero if simple vrblk, else non-zero
*      (xr,wb)               destroyed (failure case only)
*      (ra)                  destroyed
*      the expression pointer is stored in parm1 of the node
*      control returns to failp on failure of evaluation
{evalp{prc{25,r{1,1{{entry point (recursive){20675
{{mov{7,xl{13,parm1(xr){{load expression pointer{20676
{{beq{9,(xl){22,=b_exl{6,evlp1{jump if exblk case{20677
*      here for case of seblk
*      we can give a fast return if the value of the vrblk is
*      not an expression and is not trapped.
{{mov{7,xl{13,sevar(xl){{load vrblk pointer{20684
{{mov{7,xl{13,vrval(xl){{load value of vrblk{20685
{{mov{8,wa{9,(xl){{load first word of value{20686
{{bhi{8,wa{22,=b_t__{6,evlp3{jump if not seblk, trblk or exblk{20687
*      here for exblk or seblk with expr value or trapped value
{evlp1{chk{{{{check for stack space{20691
{{mov{11,-(xs){7,xr{{stack node pointer{20692
{{mov{11,-(xs){8,wb{{stack cursor{20693
{{mov{11,-(xs){3,r_pms{{stack subject string pointer{20694
{{mov{11,-(xs){3,pmssl{{stack subject string length{20695
{{mov{11,-(xs){3,pmdfl{{stack dot flag{20696
{{mov{11,-(xs){3,pmhbs{{stack history stack base pointer{20697
{{mov{7,xr{13,parm1(xr){{load expression pointer{20698
{{ejc{{{{{20699
*      evalp (continued)
*      loop back here to reevaluate expression result
{evlp2{zer{8,wb{{{set flag for by value{20705
{{jsr{6,evalx{{{evaluate expression{20706
{{ppm{6,evlp4{{{jump on failure{20707
{{mov{8,wa{9,(xr){{else load first word of value{20708
{{blo{8,wa{22,=b_e__{6,evlp2{loop back to reevaluate expression{20709
*      here to restore pattern values after successful eval
{{mov{7,xl{7,xr{{copy result pointer{20713
{{mov{3,pmhbs{10,(xs)+{{restore history stack base pointer{20714
{{mov{3,pmdfl{10,(xs)+{{restore dot flag{20715
{{mov{3,pmssl{10,(xs)+{{restore subject string length{20716
{{mov{3,r_pms{10,(xs)+{{restore subject string pointer{20717
{{mov{8,wb{10,(xs)+{{restore cursor{20718
{{mov{7,xr{10,(xs)+{{restore node pointer{20719
{{mov{8,wc{7,xr{{non-zero for simple vrblk{20720
{{exi{{{{return to evalp caller{20721
*      here to return after simple vrblk case
{evlp3{zer{8,wc{{{simple vrblk, no side effects{20725
{{exi{{{{return to evalp caller{20726
*      here for failure during evaluation
{evlp4{mov{3,pmhbs{10,(xs)+{{restore history stack base pointer{20730
{{mov{3,pmdfl{10,(xs)+{{restore dot flag{20731
{{mov{3,pmssl{10,(xs)+{{restore subject string length{20732
{{mov{3,r_pms{10,(xs)+{{restore subject string pointer{20733
{{add{7,xs{19,*num02{{remove node ptr, cursor{20734
{{exi{1,1{{{take failure exit{20735
{{enp{{{{end procedure evalp{20736
{{ejc{{{{{20737
*      evals -- evaluate string argument
*      evals is used by span, any, notany, break, breakx when
*      they are passed an expression argument.
*      (xr)                  node pointer
*      (wb)                  cursor
*      jsr  evals            call to evaluate string
*      ppm  loc              transfer loc for non-string arg
*      ppm  loc              transfer loc for evaluation failure
*      ppm  loc              transfer loc for successful eval
*      (the normal return is never taken)
*      (xr)                  ptr to node with parms set
*      (xl,wc,ra)            destroyed
*      on return, the node pointed to has a character table
*      pointer in parm1 and a bit mask in parm2. the proper
*      successor is stored in pthen of this node. thus it is
*      ok for merging with the normal (multi-char string) case.
{evals{prc{25,r{1,3{{entry point (recursive){20759
{{jsr{6,evalp{{{evaluate expression{20760
{{ppm{6,evls1{{{jump if evaluation fails{20761
{{mov{11,-(xs){13,pthen(xr){{save successor pointer{20762
{{mov{11,-(xs){8,wb{{save cursor{20763
{{mov{11,-(xs){7,xl{{stack result ptr for patst{20764
{{zer{8,wb{{{dummy pcode for one char string{20765
{{zer{8,wc{{{dummy pcode for expression arg{20766
{{mov{7,xl{22,=p_brk{{appropriate pcode for our use{20767
{{jsr{6,patst{{{call routine to build node{20768
{{ppm{6,evls2{{{jump if not string{20769
{{mov{8,wb{10,(xs)+{{restore cursor{20770
{{mov{13,pthen(xr){10,(xs)+{{store successor pointer{20771
{{exi{1,3{{{take success return{20772
*      here if evaluation fails
{evls1{exi{1,2{{{take failure return{20776
*      here if argument is not string
{evls2{add{7,xs{19,*num02{{pop successor and cursor{20780
{{exi{1,1{{{take non-string error exit{20781
{{enp{{{{end procedure evals{20782
{{ejc{{{{{20783
*      evalx -- evaluate expression
*      evalx is called to evaluate an expression
*      (xr)                  pointer to exblk or seblk
*      (wb)                  0 if by value, 1 if by name
*      jsr  evalx            call to evaluate expression
*      ppm  loc              transfer loc if evaluation fails
*      (xr)                  result if called by value
*      (xl,wa)               result name base,offset if by name
*      (xr)                  destroyed (name case only)
*      (xl,wa)               destroyed (value case only)
*      (wb,wc,ra)            destroyed
{evalx{prc{25,r{1,1{{entry point, recursive{20799
{{beq{9,(xr){22,=b_exl{6,evlx2{jump if exblk case{20800
*      here for seblk
{{mov{7,xl{13,sevar(xr){{load vrblk pointer (name base){20804
{{mov{8,wa{19,*vrval{{set name offset{20805
{{bnz{8,wb{6,evlx1{{jump if called by name{20806
{{jsr{6,acess{{{call routine to access value{20807
{{ppm{6,evlx9{{{jump if failure on access{20808
*      merge here to exit for seblk case
{evlx1{exi{{{{return to evalx caller{20812
{{ejc{{{{{20813
*      evalx (continued)
*      here for full expression (exblk) case
*      if an error occurs in the expression code at execution
*      time, control is passed via error section to exfal
*      without returning to this routine.
*      the following entries are made on the stack before
*      giving control to the expression code
*                            evalx return point
*                            saved value of r_cod
*                            code pointer (-r_cod)
*                            saved value of flptr
*                            0 if by value, 1 if by name
*      flptr --------------- *exflc, fail offset in exblk
{evlx2{scp{8,wc{{{get code pointer{20832
{{mov{8,wa{3,r_cod{{load code block pointer{20833
{{sub{8,wc{8,wa{{get code pointer as offset{20834
{{mov{11,-(xs){8,wa{{stack old code block pointer{20835
{{mov{11,-(xs){8,wc{{stack relative code offset{20836
{{mov{11,-(xs){3,flptr{{stack old failure pointer{20837
{{mov{11,-(xs){8,wb{{stack name/value indicator{20838
{{mov{11,-(xs){19,*exflc{{stack new fail offset{20839
{{mov{3,gtcef{3,flptr{{keep in case of error{20840
{{mov{3,r_gtc{3,r_cod{{keep code block pointer similarly{20841
{{mov{3,flptr{7,xs{{set new failure pointer{20842
{{mov{3,r_cod{7,xr{{set new code block pointer{20843
{{mov{13,exstm(xr){3,kvstn{{remember stmnt number{20844
{{add{7,xr{19,*excod{{point to first code word{20845
{{lcp{7,xr{{{set code pointer{20846
{{bne{3,stage{18,=stgxt{6,evlx0{jump if not execution time{20847
{{mov{3,stage{18,=stgee{{evaluating expression{20848
*      here to execute first code word of expression
{evlx0{zer{7,xl{{{clear garbage xl{20852
{{lcw{7,xr{{{load first code word{20853
{{bri{9,(xr){{{execute it{20854
{{ejc{{{{{20855
*      evalx (continued)
*      come here if successful return by value (see o_rvl)
{evlx3{mov{7,xr{10,(xs)+{{load value{20861
{{bze{13,num01(xs){6,evlx5{{jump if called by value{20862
{{erb{1,249{26,expression evaluated by name returned value{{{20863
*      here for expression returning by name (see o_rnm)
{evlx4{mov{8,wa{10,(xs)+{{load name offset{20867
{{mov{7,xl{10,(xs)+{{load name base{20868
{{bnz{13,num01(xs){6,evlx5{{jump if called by name{20869
{{jsr{6,acess{{{else access value first{20870
{{ppm{6,evlx6{{{jump if failure during access{20871
*      here after loading correct result into xr or xl,wa
{evlx5{zer{8,wb{{{note successful{20875
{{brn{6,evlx7{{{merge{20876
*      here for failure in expression evaluation (see o_fex)
{evlx6{mnz{8,wb{{{note unsuccessful{20880
*      restore environment
{evlx7{bne{3,stage{18,=stgee{6,evlx8{skip if was not previously xt{20884
{{mov{3,stage{18,=stgxt{{execute time{20885
*      merge with stage set up
{evlx8{add{7,xs{19,*num02{{pop name/value indicator, *exfal{20889
{{mov{3,flptr{10,(xs)+{{restore old failure pointer{20890
{{mov{8,wc{10,(xs)+{{load code offset{20891
{{add{8,wc{9,(xs){{make code pointer absolute{20892
{{mov{3,r_cod{10,(xs)+{{restore old code block pointer{20893
{{lcp{8,wc{{{restore old code pointer{20894
{{bze{8,wb{6,evlx1{{jump for successful return{20895
*      merge here for failure in seblk case
{evlx9{exi{1,1{{{take failure exit{20899
{{enp{{{{end of procedure evalx{20900
{{ejc{{{{{20901
*      exbld -- build exblk
*      exbld is used to build an expression block from the
*      code compiled most recently in the current ccblk.
*      (xl)                  offset in ccblk to start of code
*      (wb)                  integer in range 0 le n le mxlen
*      jsr  exbld            call to build exblk
*      (xr)                  ptr to constructed exblk
*      (wa,wb,xl)            destroyed
{exbld{prc{25,e{1,0{{entry point{20914
{{mov{8,wa{7,xl{{copy offset to start of code{20915
{{sub{8,wa{19,*excod{{calc reduction in offset in exblk{20916
{{mov{11,-(xs){8,wa{{stack for later{20917
{{mov{8,wa{3,cwcof{{load final offset{20918
{{sub{8,wa{7,xl{{compute length of code{20919
{{add{8,wa{19,*exsi_{{add space for standard fields{20920
{{jsr{6,alloc{{{allocate space for exblk{20921
{{mov{11,-(xs){7,xr{{save pointer to exblk{20922
{{mov{13,extyp(xr){22,=b_exl{{store type word{20923
{{zer{13,exstm(xr){{{zeroise stmnt number field{20924
{{mov{13,exsln(xr){3,cmpln{{set line number field{20926
{{mov{13,exlen(xr){8,wa{{store length{20928
{{mov{13,exflc(xr){21,=ofex_{{store failure word{20929
{{add{7,xr{19,*exsi_{{set xr for mvw{20930
{{mov{3,cwcof{7,xl{{reset offset to start of code{20931
{{add{7,xl{3,r_ccb{{point to start of code{20932
{{sub{8,wa{19,*exsi_{{length of code to move{20933
{{mov{11,-(xs){8,wa{{stack length of code{20934
{{mvw{{{{move code to exblk{20935
{{mov{8,wa{10,(xs)+{{get length of code{20936
{{btw{8,wa{{{convert byte count to word count{20937
{{lct{8,wa{8,wa{{prepare counter for loop{20938
{{mov{7,xl{9,(xs){{copy exblk ptr, dont unstack{20939
{{add{7,xl{19,*excod{{point to code itself{20940
{{mov{8,wb{13,num01(xs){{get reduction in offset{20941
*      this loop searches for negation and selection code so
*      that the offsets computed whilst code was in code block
*      can be transformed to reduced values applicable in an
*      exblk.
{exbl1{mov{7,xr{10,(xl)+{{get next code word{20948
{{beq{7,xr{21,=osla_{6,exbl3{jump if selection found{20949
{{beq{7,xr{21,=onta_{6,exbl3{jump if negation found{20950
{{bct{8,wa{6,exbl1{{loop to end of code{20951
*      no selection found or merge to exit on termination
{exbl2{mov{7,xr{10,(xs)+{{pop exblk ptr into xr{20955
{{mov{7,xl{10,(xs)+{{pop reduction constant{20956
{{exi{{{{return to caller{20957
{{ejc{{{{{20958
*      exbld (continued)
*      selection or negation found
*      reduce the offsets as needed. offsets occur in words
*      following code words -
*           =onta_, =osla_, =oslb_, =oslc_
{exbl3{sub{10,(xl)+{8,wb{{adjust offset{20967
{{bct{8,wa{6,exbl4{{decrement count{20968
{exbl4{bct{8,wa{6,exbl5{{decrement count{20970
*      continue search for more offsets
{exbl5{mov{7,xr{10,(xl)+{{get next code word{20974
{{beq{7,xr{21,=osla_{6,exbl3{jump if offset found{20975
{{beq{7,xr{21,=oslb_{6,exbl3{jump if offset found{20976
{{beq{7,xr{21,=oslc_{6,exbl3{jump if offset found{20977
{{beq{7,xr{21,=onta_{6,exbl3{jump if offset found{20978
{{bct{8,wa{6,exbl5{{loop{20979
{{brn{6,exbl2{{{merge to return{20980
{{enp{{{{end procedure exbld{20981
{{ejc{{{{{20982
*      expan -- analyze expression
*      the expression analyzer (expan) procedure is used to scan
*      an expression and convert it into a tree representation.
*      see the description of cmblk in the structures section
*      for detailed format of tree blocks.
*      the analyzer uses a simple precedence scheme in which
*      operands and operators are placed on a single stack
*      and condensations are made when low precedence operators
*      are stacked after a higher precedence operator. a global
*      variable (in wb) keeps track of the level as follows.
*      0    scanning outer level of statement or expression
*      1    scanning outer level of normal goto
*      2    scanning outer level of direct goto
*      3    scanning inside array brackets
*      4    scanning inside grouping parentheses
*      5    scanning inside function parentheses
*      this variable is saved on the stack on encountering a
*      grouping and restored at the end of the grouping.
*      another global variable (in wc) counts the number of
*      items at one grouping level and is incremented for each
*      comma encountered. it is stacked with the level indicator
*      the scan is controlled by a three state finite machine.
*      a global variable stored in wa is the current state.
*      wa=0                  nothing scanned at this level
*      wa=1                  operand expected
*      wa=2                  operator expected
*      (wb)                  call type (see below)
*      jsr  expan            call to analyze expression
*      (xr)                  pointer to resulting tree
*      (xl,wa,wb,wc,ra)      destroyed
*      the entry value of wb indicates the call type as follows.
*      0    scanning either the main body of a statement or the
*           text of an expression (from eval call). valid
*           terminators are colon, semicolon. the rescan flag is
*           set to return the terminator on the next scane call.
*      1    scanning a normal goto. the only valid
*           terminator is a right paren.
*      2    scanning a direct goto. the only valid
*           terminator is a right bracket.
{{ejc{{{{{21035
*      expan (continued)
*      entry point
{expan{prc{25,e{1,0{{entry point{21041
{{zer{11,-(xs){{{set top of stack indicator{21042
{{zer{8,wa{{{set initial state to zero{21043
{{zer{8,wc{{{zero counter value{21044
*      loop here for successive entries
{exp01{jsr{6,scane{{{scan next element{21048
{{add{7,xl{8,wa{{add state to syntax code{21049
{{bsw{7,xl{2,t_nes{{switch on element type/state{21050
{{iff{2,t_uo0{6,exp27{{unop, s=0{21087
{{iff{2,t_uo1{6,exp27{{unop, s=1{21087
{{iff{2,t_uo2{6,exp04{{unop, s=2{21087
{{iff{2,t_lp0{6,exp06{{left paren, s=0{21087
{{iff{2,t_lp1{6,exp06{{left paren, s=1{21087
{{iff{2,t_lp2{6,exp04{{left paren, s=2{21087
{{iff{2,t_lb0{6,exp08{{left brkt, s=0{21087
{{iff{2,t_lb1{6,exp08{{left brkt, s=1{21087
{{iff{2,t_lb2{6,exp09{{left brkt, s=2{21087
{{iff{2,t_cm0{6,exp02{{comma, s=0{21087
{{iff{2,t_cm1{6,exp05{{comma, s=1{21087
{{iff{2,t_cm2{6,exp11{{comma, s=2{21087
{{iff{2,t_fn0{6,exp10{{function, s=0{21087
{{iff{2,t_fn1{6,exp10{{function, s=1{21087
{{iff{2,t_fn2{6,exp04{{function, s=2{21087
{{iff{2,t_va0{6,exp03{{variable, s=0{21087
{{iff{2,t_va1{6,exp03{{variable, state one{21087
{{iff{2,t_va2{6,exp04{{variable, s=2{21087
{{iff{2,t_co0{6,exp03{{constant, s=0{21087
{{iff{2,t_co1{6,exp03{{constant, s=1{21087
{{iff{2,t_co2{6,exp04{{constant, s=2{21087
{{iff{2,t_bo0{6,exp05{{binop, s=0{21087
{{iff{2,t_bo1{6,exp05{{binop, s=1{21087
{{iff{2,t_bo2{6,exp26{{binop, s=2{21087
{{iff{2,t_rp0{6,exp02{{right paren, s=0{21087
{{iff{2,t_rp1{6,exp05{{right paren, s=1{21087
{{iff{2,t_rp2{6,exp12{{right paren, s=2{21087
{{iff{2,t_rb0{6,exp02{{right brkt, s=0{21087
{{iff{2,t_rb1{6,exp05{{right brkt, s=1{21087
{{iff{2,t_rb2{6,exp18{{right brkt, s=2{21087
{{iff{2,t_cl0{6,exp02{{colon, s=0{21087
{{iff{2,t_cl1{6,exp05{{colon, s=1{21087
{{iff{2,t_cl2{6,exp19{{colon, s=2{21087
{{iff{2,t_sm0{6,exp02{{semicolon, s=0{21087
{{iff{2,t_sm1{6,exp05{{semicolon, s=1{21087
{{iff{2,t_sm2{6,exp19{{semicolon, s=2{21087
{{esw{{{{end switch on element type/state{21087
{{ejc{{{{{21088
*      expan (continued)
*      here for rbr,rpr,col,smc,cma in state 0
*      set to rescan the terminator encountered and create
*      a null constant (case of omitted null)
{exp02{mnz{3,scnrs{{{set to rescan element{21097
{{mov{7,xr{21,=nulls{{point to null, merge{21098
*      here for var or con in states 0,1
*      stack the variable/constant and set state=2
{exp03{mov{11,-(xs){7,xr{{stack pointer to operand{21104
{{mov{8,wa{18,=num02{{set state 2{21105
{{brn{6,exp01{{{jump for next element{21106
*      here for var,con,lpr,fnc,uop in state 2
*      we rescan the element and create a concatenation operator
*      this is the case of the blank concatenation operator.
{exp04{mnz{3,scnrs{{{set to rescan element{21113
{{mov{7,xr{21,=opdvc{{point to concat operator dv{21114
{{bze{8,wb{6,exp4a{{ok if at top level{21115
{{mov{7,xr{21,=opdvp{{else point to unmistakable concat.{21116
*      merge here when xr set up with proper concatenation dvblk
{exp4a{bnz{3,scnbl{6,exp26{{merge bop if blanks, else error{21120
*      dcv  scnse            adjust start of element location
{{erb{1,220{26,syntax error: missing operator{{{21122
*      here for cma,rpr,rbr,col,smc,bop(s=1) bop(s=0)
*      this is an erronous contruction
*exp05 dcv  scnse            adjust start of element location
{exp05{erb{1,221{26,syntax error: missing operand{{{21130
*      here for lpr (s=0,1)
{exp06{mov{7,xl{18,=num04{{set new level indicator{21134
{{zer{7,xr{{{set zero value for cmopn{21135
{{ejc{{{{{21136
*      expan (continued)
*      merge here to store old level on stack and start new one
{exp07{mov{11,-(xs){7,xr{{stack cmopn value{21142
{{mov{11,-(xs){8,wc{{stack old counter{21143
{{mov{11,-(xs){8,wb{{stack old level indicator{21144
{{chk{{{{check for stack overflow{21145
{{zer{8,wa{{{set new state to zero{21146
{{mov{8,wb{7,xl{{set new level indicator{21147
{{mov{8,wc{18,=num01{{initialize new counter{21148
{{brn{6,exp01{{{jump to scan next element{21149
*      here for lbr (s=0,1)
*      this is an illegal use of left bracket
{exp08{erb{1,222{26,syntax error: invalid use of left bracket{{{21155
*      here for lbr (s=2)
*      set new level and start to scan subscripts
{exp09{mov{7,xr{10,(xs)+{{load array ptr for cmopn{21161
{{mov{7,xl{18,=num03{{set new level indicator{21162
{{brn{6,exp07{{{jump to stack old and start new{21163
*      here for fnc (s=0,1)
*      stack old level and start to scan arguments
{exp10{mov{7,xl{18,=num05{{set new lev indic (xr=vrblk=cmopn){21169
{{brn{6,exp07{{{jump to stack old and start new{21170
*      here for cma (s=2)
*      increment argument count and continue
{exp11{icv{8,wc{{{increment counter{21176
{{jsr{6,expdm{{{dump operators at this level{21177
{{zer{11,-(xs){{{set new level for parameter{21178
{{zer{8,wa{{{set new state{21179
{{bgt{8,wb{18,=num02{6,exp01{loop back unless outer level{21180
{{erb{1,223{26,syntax error: invalid use of comma{{{21181
{{ejc{{{{{21182
*      expan (continued)
*      here for rpr (s=2)
*      at outer level in a normal goto this is a terminator
*      otherwise it must terminate a function or grouping
{exp12{beq{8,wb{18,=num01{6,exp20{end of normal goto{21191
{{beq{8,wb{18,=num05{6,exp13{end of function arguments{21192
{{beq{8,wb{18,=num04{6,exp14{end of grouping / selection{21193
{{erb{1,224{26,syntax error: unbalanced right parenthesis{{{21194
*      here at end of function arguments
{exp13{mov{7,xl{18,=c_fnc{{set cmtyp value for function{21198
{{brn{6,exp15{{{jump to build cmblk{21199
*      here for end of grouping
{exp14{beq{8,wc{18,=num01{6,exp17{jump if end of grouping{21203
{{mov{7,xl{18,=c_sel{{else set cmtyp for selection{21204
*      merge here to build cmblk for level just scanned and
*      to pop up to the previous scan level before continuing.
{exp15{jsr{6,expdm{{{dump operators at this level{21209
{{mov{8,wa{8,wc{{copy count{21210
{{add{8,wa{18,=cmvls{{add for standard fields at start{21211
{{wtb{8,wa{{{convert length to bytes{21212
{{jsr{6,alloc{{{allocate space for cmblk{21213
{{mov{9,(xr){22,=b_cmt{{store type code for cmblk{21214
{{mov{13,cmtyp(xr){7,xl{{store cmblk node type indicator{21215
{{mov{13,cmlen(xr){8,wa{{store length{21216
{{add{7,xr{8,wa{{point past end of block{21217
{{lct{8,wc{8,wc{{set loop counter{21218
*      loop to move remaining words to cmblk
{exp16{mov{11,-(xr){10,(xs)+{{move one operand ptr from stack{21222
{{mov{8,wb{10,(xs)+{{pop to old level indicator{21223
{{bct{8,wc{6,exp16{{loop till all moved{21224
{{ejc{{{{{21225
*      expan (continued)
*      complete cmblk and stack pointer to it on stack
{{sub{7,xr{19,*cmvls{{point back to start of block{21231
{{mov{8,wc{10,(xs)+{{restore old counter{21232
{{mov{13,cmopn(xr){9,(xs){{store operand ptr in cmblk{21233
{{mov{9,(xs){7,xr{{stack cmblk pointer{21234
{{mov{8,wa{18,=num02{{set new state{21235
{{brn{6,exp01{{{back for next element{21236
*      here at end of a parenthesized expression
{exp17{jsr{6,expdm{{{dump operators at this level{21240
{{mov{7,xr{10,(xs)+{{restore xr{21241
{{mov{8,wb{10,(xs)+{{restore outer level{21242
{{mov{8,wc{10,(xs)+{{restore outer count{21243
{{mov{9,(xs){7,xr{{store opnd over unused cmopn val{21244
{{mov{8,wa{18,=num02{{set new state{21245
{{brn{6,exp01{{{back for next ele8ent{21246
*      here for rbr (s=2)
*      at outer level in a direct goto, this is a terminator.
*      otherwise it must terminate a subscript list.
{exp18{mov{7,xl{18,=c_arr{{set cmtyp for array reference{21253
{{beq{8,wb{18,=num03{6,exp15{jump to build cmblk if end arrayref{21254
{{beq{8,wb{18,=num02{6,exp20{jump if end of direct goto{21255
{{erb{1,225{26,syntax error: unbalanced right bracket{{{21256
{{ejc{{{{{21257
*      expan (continued)
*      here for col,smc (s=2)
*      error unless terminating statement body at outer level
{exp19{mnz{3,scnrs{{{rescan terminator{21265
{{mov{7,xl{8,wb{{copy level indicator{21266
{{bsw{7,xl{1,6{{switch on level indicator{21267
{{iff{1,0{6,exp20{{normal outer level{21274
{{iff{1,1{6,exp22{{fail if normal goto{21274
{{iff{1,2{6,exp23{{fail if direct goto{21274
{{iff{1,3{6,exp24{{fail array brackets{21274
{{iff{1,4{6,exp21{{fail if in grouping{21274
{{iff{1,5{6,exp21{{fail function args{21274
{{esw{{{{end switch on level{21274
*      here at normal end of expression
{exp20{jsr{6,expdm{{{dump remaining operators{21278
{{mov{7,xr{10,(xs)+{{load tree pointer{21279
{{ica{7,xs{{{pop off bottom of stack marker{21280
{{exi{{{{return to expan caller{21281
*      missing right paren
{exp21{erb{1,226{26,syntax error: missing right paren{{{21285
*      missing right paren in goto field
{exp22{erb{1,227{26,syntax error: right paren missing from goto{{{21289
*      missing bracket in goto
{exp23{erb{1,228{26,syntax error: right bracket missing from goto{{{21293
*      missing array bracket
{exp24{erb{1,229{26,syntax error: missing right array bracket{{{21297
{{ejc{{{{{21298
*      expan (continued)
*      loop here when an operator causes an operator dump
{exp25{mov{3,expsv{7,xr{{{21304
{{jsr{6,expop{{{pop one operator{21305
{{mov{7,xr{3,expsv{{restore op dv pointer and merge{21306
*      here for bop (s=2)
*      remove operators (condense) from stack until no more
*      left at this level or top one has lower precedence.
*      loop here till this condition is met.
{exp26{mov{7,xl{13,num01(xs){{load operator dvptr from stack{21314
{{ble{7,xl{18,=num05{6,exp27{jump if bottom of stack level{21315
{{blt{13,dvrpr(xr){13,dvlpr(xl){6,exp25{else pop if new prec is lo{21316
*      here for uop (s=0,1)
*      binary operator merges after precedence check
*      the operator dv is stored on the stack and the scan
*      continues after setting the scan state to one.
{exp27{mov{11,-(xs){7,xr{{stack operator dvptr on stack{21325
{{chk{{{{check for stack overflow{21326
{{mov{8,wa{18,=num01{{set new state{21327
{{bne{7,xr{21,=opdvs{6,exp01{back for next element unless ={21328
*      here for special case of binary =. the syntax allows a
*      null right argument for this operator to be left
*      out. accordingly we reset to state zero to get proper
*      action on a terminator (supply a null constant).
{{zer{8,wa{{{set state zero{21335
{{brn{6,exp01{{{jump for next element{21336
{{enp{{{{end procedure expan{21337
{{ejc{{{{{21338
*      expap -- test for pattern match tree
*      expap is passed an expression tree to determine if it
*      is a pattern match. the following are recogized as
*      matches in the context of this call.
*      1)   an explicit use of binary question mark
*      2)   a concatenation
*      3)   an alternation whose left operand is a concatenation
*      (xr)                  ptr to expan tree
*      jsr  expap            call to test for pattern match
*      ppm  loc              transfer loc if not a pattern match
*      (wa)                  destroyed
*      (xr)                  unchanged (if not match)
*      (xr)                  ptr to binary operator blk if match
{expap{prc{25,e{1,1{{entry point{21357
{{mov{11,-(xs){7,xl{{save xl{21358
{{bne{9,(xr){22,=b_cmt{6,expp2{no match if not complex{21359
{{mov{8,wa{13,cmtyp(xr){{else load type code{21360
{{beq{8,wa{18,=c_cnc{6,expp1{concatenation is a match{21361
{{beq{8,wa{18,=c_pmt{6,expp1{binary question mark is a match{21362
{{bne{8,wa{18,=c_alt{6,expp2{else not match unless alternation{21363
*      here for alternation. change (a b) / c to a qm (b / c)
{{mov{7,xl{13,cmlop(xr){{load left operand pointer{21367
{{bne{9,(xl){22,=b_cmt{6,expp2{not match if left opnd not complex{21368
{{bne{13,cmtyp(xl){18,=c_cnc{6,expp2{not match if left op not conc{21369
{{mov{13,cmlop(xr){13,cmrop(xl){{xr points to (b / c){21370
{{mov{13,cmrop(xl){7,xr{{set xl opnds to a, (b / c){21371
{{mov{7,xr{7,xl{{point to this altered node{21372
*      exit here for pattern match
{expp1{mov{7,xl{10,(xs)+{{restore entry xl{21376
{{exi{{{{give pattern match return{21377
*      exit here if not pattern match
{expp2{mov{7,xl{10,(xs)+{{restore entry xl{21381
{{exi{1,1{{{give non-match return{21382
{{enp{{{{end procedure expap{21383
{{ejc{{{{{21384
*      expdm -- dump operators at current level (for expan)
*      expdm uses expop to condense all operators at this syntax
*      level. the stack bottom is recognized from the level
*      value which is saved on the top of the stack.
*      jsr  expdm            call to dump operators
*      (xs)                  popped as required
*      (xr,wa)               destroyed
{expdm{prc{25,n{1,0{{entry point{21396
{{mov{3,r_exs{7,xl{{save xl value{21397
*      loop to dump operators
{exdm1{ble{13,num01(xs){18,=num05{6,exdm2{jump if stack bottom (saved level{21401
{{jsr{6,expop{{{else pop one operator{21402
{{brn{6,exdm1{{{and loop back{21403
*      here after popping all operators
{exdm2{mov{7,xl{3,r_exs{{restore xl{21407
{{zer{3,r_exs{{{release save location{21408
{{exi{{{{return to expdm caller{21409
{{enp{{{{end procedure expdm{21410
{{ejc{{{{{21411
*      expop-- pop operator (for expan)
*      expop is used by the expan routine to condense one
*      operator from the top of the syntax stack. an appropriate
*      cmblk is built for the operator (unary or binary) and a
*      pointer to this cmblk is stacked.
*      expop is also used by scngf (goto field scan) procedure
*      jsr  expop            call to pop operator
*      (xs)                  popped appropriately
*      (xr,xl,wa)            destroyed
{expop{prc{25,n{1,0{{entry point{21426
{{mov{7,xr{13,num01(xs){{load operator dv pointer{21427
{{beq{13,dvlpr(xr){18,=lluno{6,expo2{jump if unary{21428
*      here for binary operator
{{mov{8,wa{19,*cmbs_{{set size of binary operator cmblk{21432
{{jsr{6,alloc{{{allocate space for cmblk{21433
{{mov{13,cmrop(xr){10,(xs)+{{pop and store right operand ptr{21434
{{mov{7,xl{10,(xs)+{{pop and load operator dv ptr{21435
{{mov{13,cmlop(xr){9,(xs){{store left operand pointer{21436
*      common exit point
{expo1{mov{9,(xr){22,=b_cmt{{store type code for cmblk{21440
{{mov{13,cmtyp(xr){13,dvtyp(xl){{store cmblk node type code{21441
{{mov{13,cmopn(xr){7,xl{{store dvptr (=ptr to dac o_xxx){21442
{{mov{13,cmlen(xr){8,wa{{store cmblk length{21443
{{mov{9,(xs){7,xr{{store resulting node ptr on stack{21444
{{exi{{{{return to expop caller{21445
*      here for unary operator
{expo2{mov{8,wa{19,*cmus_{{set size of unary operator cmblk{21449
{{jsr{6,alloc{{{allocate space for cmblk{21450
{{mov{13,cmrop(xr){10,(xs)+{{pop and store operand pointer{21451
{{mov{7,xl{9,(xs){{load operator dv pointer{21452
{{brn{6,expo1{{{merge back to exit{21453
{{enp{{{{end procedure expop{21454
{{ejc{{{{{21455
*      filnm -- obtain file name from statement number
*      filnm takes a statement number and examines the file name
*      table pointed to by r_sfn to find the name of the file
*      containing the given statement.  table entries are
*      arranged in order of ascending statement number (there
*      is only one hash bucket in this table).  elements are
*      added to the table each time there is a change in
*      file name, recording the then current statement number.
*      to find the file name, the linked list of teblks is
*      scanned for an element containing a subscript (statement
*      number) greater than the argument statement number, or
*      the end of chain.  when this condition is met, the
*      previous teblk contains the desired file name as its
*      value entry.
*      (wc)                  statement number
*      jsr  filnm            call to obtain file name
*      (xl)                  file name (scblk)
*      (ia)                  destroyed
{filnm{prc{25,e{1,0{{entry point{21480
{{mov{11,-(xs){8,wb{{preserve wb{21481
{{bze{8,wc{6,filn3{{return nulls if stno is zero{21482
{{mov{7,xl{3,r_sfn{{file name table{21483
{{bze{7,xl{6,filn3{{if no table{21484
{{mov{8,wb{13,tbbuk(xl){{get bucket entry{21485
{{beq{8,wb{3,r_sfn{6,filn3{jump if no teblks on chain{21486
{{mov{11,-(xs){7,xr{{preserve xr{21487
{{mov{7,xr{8,wb{{previous block pointer{21488
{{mov{11,-(xs){8,wc{{preserve stmt number{21489
*      loop through teblks on hash chain
{filn1{mov{7,xl{7,xr{{next element to examine{21493
{{mov{7,xr{13,tesub(xl){{load subscript value (an icblk){21494
{{ldi{13,icval(xr){{{load the statement number{21495
{{mfi{8,wc{{{convert to address constant{21496
{{blt{9,(xs){8,wc{6,filn2{compare arg with teblk stmt number{21497
*      here if desired stmt number is ge teblk stmt number
{{mov{8,wb{7,xl{{save previous entry pointer{21501
{{mov{7,xr{13,tenxt(xl){{point to next teblk on chain{21502
{{bne{7,xr{3,r_sfn{6,filn1{jump if there is one{21503
*      here if chain exhausted or desired block found.
{filn2{mov{7,xl{8,wb{{previous teblk{21507
{{mov{7,xl{13,teval(xl){{get ptr to file name scblk{21508
{{mov{8,wc{10,(xs)+{{restore stmt number{21509
{{mov{7,xr{10,(xs)+{{restore xr{21510
{{mov{8,wb{10,(xs)+{{restore wb{21511
{{exi{{{{{21512
*      no table or no table entries
{filn3{mov{8,wb{10,(xs)+{{restore wb{21516
{{mov{7,xl{21,=nulls{{return null string{21517
{{exi{{{{{21518
{{enp{{{{{21519
{{ejc{{{{{21520
*      gbcol -- perform garbage collection
*      gbcol performs a garbage collection on the dynamic region
*      all blocks which are no longer in use are eliminated
*      by moving blocks which are in use down and resetting
*      dnamp, the pointer to the next available location.
*      (wb)                  move offset (see below)
*      jsr  gbcol            call to collect garbage
*      (xr)                  sediment size after collection
*      the following conditions must be met at the time when
*      gbcol is called.
*      1)   all pointers to blocks in the dynamic area must be
*           accessible to the garbage collector. this means
*           that they must occur in one of the following.
*           a)               main stack, with current top
*                            element being indicated by xs
*           b)               in relocatable fields of vrblks.
*           c)               in register xl at the time of call
*           e)               in the special region of working
*                            storage where names begin with r_.
*      2)   all pointers must point to the start of blocks with
*           the sole exception of the contents of the code
*           pointer register which points into the r_cod block.
*      3)   no location which appears to contain a pointer
*           into the dynamic region may occur unless it is in
*           fact a pointer to the start of the block. however
*           pointers outside this area may occur and will
*           not be changed by the garbage collector.
*           it is especially important to make sure that xl
*           does not contain a garbage value from some process
*           carried out before the call to the collector.
*      gbcol has the capability of moving the final compacted
*      result up in memory (with addresses adjusted accordingly)
*      this is used to add space to the static region. the
*      entry value of wb is the number of bytes to move up.
*      the caller must guarantee that there is enough room.
*      furthermore the value in wb if it is non-zero, must be at
*      least 256 so that the mwb instruction conditions are met.
{{ejc{{{{{21622
*      gbcol (continued)
*      the algorithm, which is a modification of the lisp-2
*      garbage collector devised by r.dewar and k.belcher
*      takes three passes as follows.
*      1)   all pointers in memory are scanned and blocks in use
*           determined from this scan. note that this procedure
*           is recursive and uses the main stack for linkage.
*           the marking process is thus similar to that used in
*           a standard lisp collector. however the method of
*           actually marking the blocks is different.
*           the first field of a block normally contains a
*           code entry point pointer. such an entry pointer
*           can be distinguished from the address of any pointer
*           to be processed by the collector. during garbage
*           collection, this word is used to build a back chain
*           of pointers through fields which point to the block.
*           the end of the chain is marked by the occurence
*           of the word which used to be in the first word of
*           the block. this backchain serves both as a mark
*           indicating that the block is in use and as a list of
*           references for the relocation phase.
*      2)   storage is scanned sequentially to discover which
*           blocks are currently in use as indicated by the
*           presence of a backchain. two pointers are maintained
*           one scans through looking at each block. the other
*           is incremented only for blocks found to be in use.
*           in this way, the eventual location of each block can
*           be determined without actually moving any blocks.
*           as each block which is in use is processed, the back
*           chain is used to reset all pointers which point to
*           this block to contain its new address, i.e. the
*           address it will occupy after the blocks are moved.
*           the first word of the block, taken from the end of
*           the chain is restored at this point.
*           during pass 2, the collector builds blocks which
*           describe the regions of storage which are to be
*           moved in the third pass. there is one descriptor for
*           each contiguous set of good blocks. the descriptor
*           is built just behind the block to be moved and
*           contains a pointer to the next block and the number
*           of words to be moved.
*      3)   in the third and final pass, the move descriptor
*           blocks built in pass two are used to actually move
*           the blocks down to the bottom of the dynamic region.
*           the collection is then complete and the next
*           available location pointer is reset.
{{ejc{{{{{21676
*      gbcol (continued)
*      the garbage collector also recognizes the concept of
*      sediment.  sediment is defined as long-lived objects
*      which percipitate to the bottom of dynamic storage.
*      moving these objects during repeated collections is
*      inefficient.  it also contributes to thrashing on
*      systems with virtual memory.  in a typical worst-case
*      situation, there may be several megabytes of live objects
*      in the sediment, and only a few dead objects in need of
*      collection.  without recognising sediment, the standard
*      collector would move those megabytes of objects downward
*      to squeeze out the dead objects.  this type of move
*      would result in excessive thrasing for very little memory
*      gain.
*      scanning of blocks in the sediment cannot be avoided
*      entirely, because these blocks may contain pointers to
*      live objects above the sediment.  however, sediment
*      blocks need not be linked to a back chain as described
*      in pass one above.  since these blocks will not be moved,
*      pointers to them do not need to be adjusted.  eliminating
*      unnecessary back chain links increases locality of
*      reference, improving virtual memory performance.
*      because back chains are used to mark blocks whose con-
*      tents have been processed, a different marking system
*      is needed for blocks in the sediment.  since block type
*      words point to odd-parity entry addresses, merely incre-
*      menting the type word serves to mark the block as pro-
*      cessed.  during pass three, the type words are decre-
*      mented to restore them to their original value.
{{ejc{{{{{21720
*      gbcol (continued)
*      the variable dnams contains the number of bytes of memory
*      currently in the sediment.  setting dnams to zero will
*      eliminate the sediment and force it to be included in a
*      full garbage collection.  gbcol returns a suggested new
*      value for dnams (usually dnamp-dnamb) in xr which the
*      caller can store in dnams if it wishes to maintain the
*      sediment.  that is, data remaining after a garbage
*      collection is considered to be sediment.  if one accepts
*      the common lore that most objects are either very short-
*      or very long-lived, then this naive setting of dnams
*      probably includes some short-lived objects toward the end
*      of the sediment.
*      knowing when to reset dnams to zero to collect the sedi-
*      ment is not precisely known.  we force it to zero prior
*      to producing a dump, when gbcol is invoked by collect()
*      (so that the sediment is invisible to the user), when
*      sysmm is unable to obtain additional memory, and when
*      gbcol is called to relocate the dynamic area up in memory
*      (to make room for enlarging the static area).  if there
*      are no other reset situations, this leads to the inexo-
*      rable growth of the sediment, possible forcing a modest
*      program to begin to use virtual memory that it otherwise
*      would not.
*      as we scan sediment blocks in pass three, we maintain
*      aggregate counts of the amount of dead and live storage,
*      which is used to decide when to reset dnams.  when the
*      ratio of free storage found in the sediment to total
*      sediment size exceeds a threshold, the sediment is marked
*      for collection on the next gbcol call.
{{ejc{{{{{21758
*      gbcol (continued)
{gbcol{prc{25,e{1,0{{entry point{21762
*z-
{{bnz{3,dmvch{6,gbc14{{fail if in mid-dump{21764
{{mnz{3,gbcfl{{{note gbcol entered{21765
{{mov{3,gbsva{8,wa{{save entry wa{21766
{{mov{3,gbsvb{8,wb{{save entry wb{21767
{{mov{3,gbsvc{8,wc{{save entry wc{21768
{{mov{11,-(xs){7,xl{{save entry xl{21769
{{scp{8,wa{{{get code pointer value{21770
{{sub{8,wa{3,r_cod{{make relative{21771
{{lcp{8,wa{{{and restore{21772
{{bze{8,wb{6,gbc0a{{check there is no move offset{21774
{{zer{3,dnams{{{collect sediment if must move it{21775
{gbc0a{mov{8,wa{3,dnamb{{start of dynamic area{21776
{{add{8,wa{3,dnams{{size of sediment{21777
{{mov{3,gbcsd{8,wa{{first location past sediment{21778
*      inform sysgc that collection to commence
{{mnz{7,xr{{{non-zero flags start of collection{21791
{{mov{8,wa{3,dnamb{{start of dynamic area{21792
{{mov{8,wb{3,dnamp{{next available location{21793
{{mov{8,wc{3,dname{{last available location + 1{21794
{{jsr{6,sysgc{{{inform of collection{21795
*      process stack entries
{{mov{7,xr{7,xs{{point to stack front{21800
{{mov{7,xl{3,stbas{{point past end of stack{21801
{{bge{7,xl{7,xr{6,gbc00{ok if d-stack{21802
{{mov{7,xr{7,xl{{reverse if ...{21803
{{mov{7,xl{7,xs{{... u-stack{21804
*      process the stack
{gbc00{jsr{6,gbcpf{{{process pointers on stack{21808
*      process special work locations
{{mov{7,xr{20,=r_aaa{{point to start of relocatable locs{21812
{{mov{7,xl{20,=r_yyy{{point past end of relocatable locs{21813
{{jsr{6,gbcpf{{{process work fields{21814
*      prepare to process variable blocks
{{mov{8,wa{3,hshtb{{point to first hash slot pointer{21818
*      loop through hash slots
{gbc01{mov{7,xl{8,wa{{point to next slot{21822
{{ica{8,wa{{{bump bucket pointer{21823
{{mov{3,gbcnm{8,wa{{save bucket pointer{21824
{{ejc{{{{{21825
*      gbcol (continued)
*      loop through variables on one hash chain
{gbc02{mov{7,xr{9,(xl){{load ptr to next vrblk{21831
{{bze{7,xr{6,gbc03{{jump if end of chain{21832
{{mov{7,xl{7,xr{{else copy vrblk pointer{21833
{{add{7,xr{19,*vrval{{point to first reloc fld{21834
{{add{7,xl{19,*vrnxt{{point past last (and to link ptr){21835
{{jsr{6,gbcpf{{{process reloc fields in vrblk{21836
{{brn{6,gbc02{{{loop back for next block{21837
*      here at end of one hash chain
{gbc03{mov{8,wa{3,gbcnm{{restore bucket pointer{21841
{{bne{8,wa{3,hshte{6,gbc01{loop back if more buckets to go{21842
{{ejc{{{{{21843
*      gbcol (continued)
*      now we are ready to start pass two. registers are used
*      as follows in pass two.
*      (xr)                  scans through all blocks
*      (wc)                  pointer to eventual location
*      the move description blocks built in this pass have
*      the following format.
*      word 1                pointer to next move block,
*                            zero if end of chain of blocks
*      word 2                length of blocks to be moved in
*                            bytes. set to the address of the
*                            first byte while actually scanning
*                            the blocks.
*      the first entry on this chain is a special entry
*      consisting of the two words gbcnm and gbcns. after
*      building the chain of move descriptors, gbcnm points to
*      the first real move block, and gbcns is the length of
*      blocks in use at the start of storage which need not
*      be moved since they are in the correct position.
{{mov{7,xr{3,dnamb{{point to first block{21872
{{zer{8,wb{{{accumulate size of dead blocks{21873
{gbc04{beq{7,xr{3,gbcsd{6,gbc4c{jump if end of sediment{21874
{{mov{8,wa{9,(xr){{else get first word{21875
{{bod{8,wa{6,gbc4b{{jump if entry pointer (unused){21877
{{dcv{8,wa{{{restore entry pointer{21878
{{mov{9,(xr){8,wa{{restore first word{21884
{{jsr{6,blkln{{{get length of this block{21885
{{add{7,xr{8,wa{{bump actual pointer{21886
{{brn{6,gbc04{{{continue scan through sediment{21887
*      here for unused sediment block
{gbc4b{jsr{6,blkln{{{get length of this block{21891
{{add{7,xr{8,wa{{bump actual pointer{21892
{{add{8,wb{8,wa{{count size of unused blocks{21893
{{brn{6,gbc04{{{continue scan through sediment{21894
*      here at end of sediment.  remember size of free blocks
*      within the sediment.  this will be used later to decide
*      how to set the sediment size returned to caller.
*      then scan rest of dynamic area above sediment.
*      (wb) = aggregate size of free blocks in sediment
*      (xr) = first location past sediment
{gbc4c{mov{3,gbcsf{8,wb{{size of sediment free space{21905
{{mov{8,wc{7,xr{{set as first eventual location{21909
{{add{8,wc{3,gbsvb{{add offset for eventual move up{21910
{{zer{3,gbcnm{{{clear initial forward pointer{21911
{{mov{3,gbclm{20,=gbcnm{{initialize ptr to last move block{21912
{{mov{3,gbcns{7,xr{{initialize first address{21913
*      loop through a series of blocks in use
{gbc05{beq{7,xr{3,dnamp{6,gbc07{jump if end of used region{21917
{{mov{8,wa{9,(xr){{else get first word{21918
{{bod{8,wa{6,gbc07{{jump if entry pointer (unused){21920
*      here for block in use, loop to relocate references
{gbc06{mov{7,xl{8,wa{{copy pointer{21928
{{mov{8,wa{9,(xl){{load forward pointer{21929
{{mov{9,(xl){8,wc{{relocate reference{21930
{{bev{8,wa{6,gbc06{{loop back if not end of chain{21932
{{ejc{{{{{21937
*      gbcol (continued)
*      at end of chain, restore first word and bump past
{{mov{9,(xr){8,wa{{restore first word{21943
{{jsr{6,blkln{{{get length of this block{21944
{{add{7,xr{8,wa{{bump actual pointer{21945
{{add{8,wc{8,wa{{bump eventual pointer{21946
{{brn{6,gbc05{{{loop back for next block{21947
*      here at end of a series of blocks in use
{gbc07{mov{8,wa{7,xr{{copy pointer past last block{21951
{{mov{7,xl{3,gbclm{{point to previous move block{21952
{{sub{8,wa{13,num01(xl){{subtract starting address{21953
{{mov{13,num01(xl){8,wa{{store length of block to be moved{21954
*      loop through a series of blocks not in use
{gbc08{beq{7,xr{3,dnamp{6,gbc10{jump if end of used region{21958
{{mov{8,wa{9,(xr){{else load first word of next block{21959
{{bev{8,wa{6,gbc09{{jump if in use{21961
{{jsr{6,blkln{{{else get length of next block{21966
{{add{7,xr{8,wa{{push pointer{21967
{{brn{6,gbc08{{{and loop back{21968
*      here for a block in use after processing a series of
*      blocks which were not in use, build new move block.
{gbc09{sub{7,xr{19,*num02{{point 2 words behind for move block{21973
{{mov{7,xl{3,gbclm{{point to previous move block{21974
{{mov{9,(xl){7,xr{{set forward ptr in previous block{21975
{{zer{9,(xr){{{zero forward ptr of new block{21976
{{mov{3,gbclm{7,xr{{remember address of this block{21977
{{mov{7,xl{7,xr{{copy ptr to move block{21978
{{add{7,xr{19,*num02{{point back to block in use{21979
{{mov{13,num01(xl){7,xr{{store starting address{21980
{{brn{6,gbc06{{{jump to process block in use{21981
{{ejc{{{{{21982
*      gbcol (continued)
*      here for pass three -- actually move the blocks down
*      (xl)                  pointer to old location
*      (xr)                  pointer to new location
{gbc10{mov{7,xr{3,gbcsd{{point to storage above sediment{21992
{{add{7,xr{3,gbcns{{bump past unmoved blocks at start{21996
*      loop through move descriptors
{gbc11{mov{7,xl{3,gbcnm{{point to next move block{22000
{{bze{7,xl{6,gbc12{{jump if end of chain{22001
{{mov{3,gbcnm{10,(xl)+{{move pointer down chain{22002
{{mov{8,wa{10,(xl)+{{get length to move{22003
{{mvw{{{{perform move{22004
{{brn{6,gbc11{{{loop back{22005
*      now test for move up
{gbc12{mov{3,dnamp{7,xr{{set next available loc ptr{22009
{{mov{8,wb{3,gbsvb{{reload move offset{22010
{{bze{8,wb{6,gbc13{{jump if no move required{22011
{{mov{7,xl{7,xr{{else copy old top of core{22012
{{add{7,xr{8,wb{{point to new top of core{22013
{{mov{3,dnamp{7,xr{{save new top of core pointer{22014
{{mov{8,wa{7,xl{{copy old top{22015
{{sub{8,wa{3,dnamb{{minus old bottom = length{22016
{{add{3,dnamb{8,wb{{bump bottom to get new value{22017
{{mwb{{{{perform move (backwards){22018
*      merge here to exit
{gbc13{zer{7,xr{{{clear garbage value in xr{22022
{{mov{3,gbcfl{7,xr{{note exit from gbcol{22023
{{mov{8,wa{3,dnamb{{start of dynamic area{22025
{{mov{8,wb{3,dnamp{{next available location{22026
{{mov{8,wc{3,dname{{last available location + 1{22027
{{jsr{6,sysgc{{{inform sysgc of completion{22028
*      decide whether to mark sediment for collection next time.
*      this is done by examining the ratio of previous sediment
*      free space to the new sediment size.
{{sti{3,gbcia{{{save ia{22036
{{zer{7,xr{{{presume no sediment will remain{22037
{{mov{8,wb{3,gbcsf{{free space in sediment{22038
{{btw{8,wb{{{convert bytes to words{22039
{{mti{8,wb{{{put sediment free store in ia{22040
{{mli{3,gbsed{{{multiply by sediment factor{22041
{{iov{6,gb13a{{{jump if overflowed{22042
{{mov{8,wb{3,dnamp{{end of dynamic area in use{22043
{{sub{8,wb{3,dnamb{{minus start is sediment remaining{22044
{{btw{8,wb{{{convert to words{22045
{{mov{3,gbcsf{8,wb{{store it{22046
{{sbi{3,gbcsf{{{subtract from scaled up free store{22047
{{igt{6,gb13a{{{jump if large free store in sedimnt{22048
{{mov{7,xr{3,dnamp{{below threshold, return sediment{22049
{{sub{7,xr{3,dnamb{{for use by caller{22050
{gb13a{ldi{3,gbcia{{{restore ia{22051
{{mov{8,wa{3,gbsva{{restore wa{22053
{{mov{8,wb{3,gbsvb{{restore wb{22054
{{scp{8,wc{{{get code pointer{22055
{{add{8,wc{3,r_cod{{make absolute again{22056
{{lcp{8,wc{{{and replace absolute value{22057
{{mov{8,wc{3,gbsvc{{restore wc{22058
{{mov{7,xl{10,(xs)+{{restore entry xl{22059
{{icv{3,gbcnt{{{increment count of collections{22060
{{exi{{{{exit to gbcol caller{22061
*      garbage collection not allowed whilst dumping
{gbc14{icv{3,errft{{{fatal error{22065
{{erb{1,250{26,insufficient memory to complete dump{{{22066
{{enp{{{{end procedure gbcol{22067
{{ejc{{{{{22068
*      gbcpf -- process fields for garbage collector
*      this procedure is used by the garbage collector to
*      process fields in pass one. see gbcol for full details.
*      (xr)                  ptr to first location to process
*      (xl)                  ptr past last location to process
*      jsr  gbcpf            call to process fields
*      (xr,wa,wb,wc,ia)      destroyed
*      note that although this procedure uses a recursive
*      approach, it controls its own stack and is not recursive.
{gbcpf{prc{25,e{1,0{{entry point{22083
{{zer{11,-(xs){{{set zero to mark bottom of stack{22084
{{mov{11,-(xs){7,xl{{save end pointer{22085
*      merge here to go down a level and start a new loop
*      1(xs)                 next lvl field ptr (0 at outer lvl)
*      0(xs)                 ptr past last field to process
*      (xr)                  ptr to first field to process
*      loop to process successive fields
{gpf01{mov{7,xl{9,(xr){{load field contents{22095
{{mov{8,wc{7,xr{{save field pointer{22096
{{blt{7,xl{3,dnamb{6,gpf2a{jump if not ptr into dynamic area{22100
{{bge{7,xl{3,dnamp{6,gpf2a{jump if not ptr into dynamic area{22101
*      here we have a ptr to a block in the dynamic area.
*      link this field onto the reference backchain.
{{mov{8,wa{9,(xl){{load ptr to chain (or entry ptr){22106
{{blt{7,xl{3,gbcsd{6,gpf1a{do not chain if within sediment{22108
{{mov{9,(xl){7,xr{{set this field as new head of chain{22110
{{mov{9,(xr){8,wa{{set forward pointer{22111
*      now see if this block has been processed before
{gpf1a{bod{8,wa{6,gpf03{{jump if not already processed{22116
*      here to restore pointer in xr to field just processed
{gpf02{mov{7,xr{8,wc{{restore field pointer{22124
*      here to move to next field
{gpf2a{ica{7,xr{{{bump to next field{22128
{{bne{7,xr{9,(xs){6,gpf01{loop back if more to go{22129
{{ejc{{{{{22130
*      gbcpf (continued)
*      here we pop up a level after finishing a block
{{mov{7,xl{10,(xs)+{{restore pointer past end{22136
{{mov{7,xr{10,(xs)+{{restore block pointer{22137
{{bnz{7,xr{6,gpf2a{{continue loop unless outer levl{22138
{{exi{{{{return to caller if outer level{22139
*      here to process an active block which has not been done
*      since sediment blocks are not marked by putting them on
*      the back chain, they must be explicitly marked in another
*      manner.  if odd parity entry points are present, mark by
*      temporarily converting to even parity.  if odd parity not
*      available, the entry point is adjusted by the value in
*      gbcmk.
{gpf03{bge{7,xl{3,gbcsd{6,gpf3a{if not within sediment{22152
{{icv{9,(xl){{{mark by making entry point even{22154
{gpf3a{mov{7,xr{7,xl{{copy block pointer{22158
{{mov{7,xl{8,wa{{copy first word of block{22162
{{lei{7,xl{{{load entry point id (bl_xx){22163
*      block type switch. note that blocks with no relocatable
*      fields just return to gpf02 here to continue to next fld.
{{bsw{7,xl{2,bl___{{switch on block type{22168
{{iff{2,bl_ar{6,gpf06{{arblk{22206
{{iff{2,bl_cd{6,gpf19{{cdblk{22206
{{iff{2,bl_ex{6,gpf17{{exblk{22206
{{iff{2,bl_ic{6,gpf02{{icblk{22206
{{iff{2,bl_nm{6,gpf10{{nmblk{22206
{{iff{2,bl_p0{6,gpf10{{p0blk{22206
{{iff{2,bl_p1{6,gpf12{{p1blk{22206
{{iff{2,bl_p2{6,gpf12{{p2blk{22206
{{iff{2,bl_rc{6,gpf02{{rcblk{22206
{{iff{2,bl_sc{6,gpf02{{scblk{22206
{{iff{2,bl_se{6,gpf02{{seblk{22206
{{iff{2,bl_tb{6,gpf08{{tbblk{22206
{{iff{2,bl_vc{6,gpf08{{vcblk{22206
{{iff{2,bl_xn{6,gpf02{{xnblk{22206
{{iff{2,bl_xr{6,gpf09{{xrblk{22206
{{iff{2,bl_bc{6,gpf02{{bcblk - dummy to fill out iffs{22206
{{iff{2,bl_pd{6,gpf13{{pdblk{22206
{{iff{2,bl_tr{6,gpf16{{trblk{22206
{{iff{2,bl_bf{6,gpf02{{bfblk{22206
{{iff{2,bl_cc{6,gpf07{{ccblk{22206
{{iff{2,bl_cm{6,gpf04{{cmblk{22206
{{iff{2,bl_ct{6,gpf02{{ctblk{22206
{{iff{2,bl_df{6,gpf02{{dfblk{22206
{{iff{2,bl_ef{6,gpf02{{efblk{22206
{{iff{2,bl_ev{6,gpf10{{evblk{22206
{{iff{2,bl_ff{6,gpf11{{ffblk{22206
{{iff{2,bl_kv{6,gpf02{{kvblk{22206
{{iff{2,bl_pf{6,gpf14{{pfblk{22206
{{iff{2,bl_te{6,gpf15{{teblk{22206
{{esw{{{{end of jump table{22206
{{ejc{{{{{22207
*      gbcpf (continued)
*      cmblk
{gpf04{mov{8,wa{13,cmlen(xr){{load length{22213
{{mov{8,wb{19,*cmtyp{{set offset{22214
*      here to push down to new level
*      (wc)                  field ptr at previous level
*      (xr)                  ptr to new block
*      (wa)                  length (reloc flds + flds at start)
*      (wb)                  offset to first reloc field
{gpf05{add{8,wa{7,xr{{point past last reloc field{22223
{{add{7,xr{8,wb{{point to first reloc field{22224
{{mov{11,-(xs){8,wc{{stack old field pointer{22225
{{mov{11,-(xs){8,wa{{stack new limit pointer{22226
{{chk{{{{check for stack overflow{22227
{{brn{6,gpf01{{{if ok, back to process{22228
*      arblk
{gpf06{mov{8,wa{13,arlen(xr){{load length{22232
{{mov{8,wb{13,arofs(xr){{set offset to 1st reloc fld (arpro){22233
{{brn{6,gpf05{{{all set{22234
*      ccblk
{gpf07{mov{8,wa{13,ccuse(xr){{set length in use{22238
{{mov{8,wb{19,*ccuse{{1st word (make sure at least one){22239
{{brn{6,gpf05{{{all set{22240
{{ejc{{{{{22241
*      gbcpf (continued)
*      cdblk
{gpf19{mov{8,wa{13,cdlen(xr){{load length{22248
{{mov{8,wb{19,*cdfal{{set offset{22249
{{brn{6,gpf05{{{jump back{22250
*      tbblk, vcblk
{gpf08{mov{8,wa{13,offs2(xr){{load length{22257
{{mov{8,wb{19,*offs3{{set offset{22258
{{brn{6,gpf05{{{jump back{22259
*      xrblk
{gpf09{mov{8,wa{13,xrlen(xr){{load length{22263
{{mov{8,wb{19,*xrptr{{set offset{22264
{{brn{6,gpf05{{{jump back{22265
*      evblk, nmblk, p0blk
{gpf10{mov{8,wa{19,*offs2{{point past second field{22269
{{mov{8,wb{19,*offs1{{offset is one (only reloc fld is 2){22270
{{brn{6,gpf05{{{all set{22271
*      ffblk
{gpf11{mov{8,wa{19,*ffofs{{set length{22275
{{mov{8,wb{19,*ffnxt{{set offset{22276
{{brn{6,gpf05{{{all set{22277
*      p1blk, p2blk
{gpf12{mov{8,wa{19,*parm2{{length (parm2 is non-relocatable){22281
{{mov{8,wb{19,*pthen{{set offset{22282
{{brn{6,gpf05{{{all set{22283
{{ejc{{{{{22284
*      gbcpf (continued)
*      pdblk
{gpf13{mov{7,xl{13,pddfp(xr){{load ptr to dfblk{22290
{{mov{8,wa{13,dfpdl(xl){{get pdblk length{22291
{{mov{8,wb{19,*pdfld{{set offset{22292
{{brn{6,gpf05{{{all set{22293
*      pfblk
{gpf14{mov{8,wa{19,*pfarg{{length past last reloc{22297
{{mov{8,wb{19,*pfcod{{offset to first reloc{22298
{{brn{6,gpf05{{{all set{22299
*      teblk
{gpf15{mov{8,wa{19,*tesi_{{set length{22303
{{mov{8,wb{19,*tesub{{and offset{22304
{{brn{6,gpf05{{{all set{22305
*      trblk
{gpf16{mov{8,wa{19,*trsi_{{set length{22309
{{mov{8,wb{19,*trval{{and offset{22310
{{brn{6,gpf05{{{all set{22311
*      exblk
{gpf17{mov{8,wa{13,exlen(xr){{load length{22315
{{mov{8,wb{19,*exflc{{set offset{22316
{{brn{6,gpf05{{{jump back{22317
{{enp{{{{end procedure gbcpf{22327
{{ejc{{{{{22328
*z+
*      gtarr -- get array
*      gtarr is passed an object and returns an array if possibl
*      (xr)                  value to be converted
*      (wa)                  0 to place table addresses in array
*                            non-zero for keys/values in array
*      jsr  gtarr            call to get array
*      ppm  loc              transfer loc for all null table
*      ppm  loc              transfer loc if convert impossible
*      (xr)                  resulting array
*      (xl,wa,wb,wc)         destroyed
{gtarr{prc{25,e{1,2{{entry point{22344
{{mov{3,gtawa{8,wa{{save wa indicator{22345
{{mov{8,wa{9,(xr){{load type word{22346
{{beq{8,wa{22,=b_art{6,gtar8{exit if already an array{22347
{{beq{8,wa{22,=b_vct{6,gtar8{exit if already an array{22348
{{bne{8,wa{22,=b_tbt{6,gta9a{else fail if not a table (sgd02){22349
*      here we convert a table to an array
{{mov{11,-(xs){7,xr{{replace tbblk pointer on stack{22353
{{zer{7,xr{{{signal first pass{22354
{{zer{8,wb{{{zero non-null element count{22355
*      the following code is executed twice. on the first pass,
*      signalled by xr=0, the number of non-null elements in
*      the table is counted in wb. in the second pass, where
*      xr is a pointer into the arblk, the name and value are
*      entered into the current arblk location provided gtawa
*      is non-zero.  if gtawa is zero, the address of the teblk
*      is entered into the arblk twice (c3.762).
{gtar1{mov{7,xl{9,(xs){{point to table{22365
{{add{7,xl{13,tblen(xl){{point past last bucket{22366
{{sub{7,xl{19,*tbbuk{{set first bucket offset{22367
{{mov{8,wa{7,xl{{copy adjusted pointer{22368
*      loop through buckets in table block
*      next three lines of code rely on tenxt having a value
*      1 less than tbbuk.
{gtar2{mov{7,xl{8,wa{{copy bucket pointer{22374
{{dca{8,wa{{{decrement bucket pointer{22375
*      loop through teblks on one bucket chain
{gtar3{mov{7,xl{13,tenxt(xl){{point to next teblk{22379
{{beq{7,xl{9,(xs){6,gtar6{jump if chain end (tbblk ptr){22380
{{mov{3,cnvtp{7,xl{{else save teblk pointer{22381
*      loop to find value down trblk chain
{gtar4{mov{7,xl{13,teval(xl){{load value{22385
{{beq{9,(xl){22,=b_trt{6,gtar4{loop till value found{22386
{{mov{8,wc{7,xl{{copy value{22387
{{mov{7,xl{3,cnvtp{{restore teblk pointer{22388
{{ejc{{{{{22389
*      gtarr (continued)
*      now check for null and test cases
{{beq{8,wc{21,=nulls{6,gtar3{loop back to ignore null value{22395
{{bnz{7,xr{6,gtar5{{jump if second pass{22396
{{icv{8,wb{{{for the first pass, bump count{22397
{{brn{6,gtar3{{{and loop back for next teblk{22398
*      here in second pass
{gtar5{bze{3,gtawa{6,gta5a{{jump if address wanted{22402
{{mov{10,(xr)+{13,tesub(xl){{store subscript name{22403
{{mov{10,(xr)+{8,wc{{store value in arblk{22404
{{brn{6,gtar3{{{loop back for next teblk{22405
*      here to record teblk address in arblk.  this allows
*      a sort routine to sort by ascending address.
{gta5a{mov{10,(xr)+{7,xl{{store teblk address in name{22410
{{mov{10,(xr)+{7,xl{{and value slots{22411
{{brn{6,gtar3{{{loop back for next teblk{22412
*      here after scanning teblks on one chain
{gtar6{bne{8,wa{9,(xs){6,gtar2{loop back if more buckets to go{22416
{{bnz{7,xr{6,gtar7{{else jump if second pass{22417
*      here after counting non-null elements
{{bze{8,wb{6,gtar9{{fail if no non-null elements{22421
{{mov{8,wa{8,wb{{else copy count{22422
{{add{8,wa{8,wb{{double (two words/element){22423
{{add{8,wa{18,=arvl2{{add space for standard fields{22424
{{wtb{8,wa{{{convert length to bytes{22425
{{bgt{8,wa{3,mxlen{6,gta9b{error if too long for array{22426
{{jsr{6,alloc{{{else allocate space for arblk{22427
{{mov{9,(xr){22,=b_art{{store type word{22428
{{zer{13,idval(xr){{{zero id for the moment{22429
{{mov{13,arlen(xr){8,wa{{store length{22430
{{mov{13,arndm(xr){18,=num02{{set dimensions = 2{22431
{{ldi{4,intv1{{{get integer one{22432
{{sti{13,arlbd(xr){{{store as lbd 1{22433
{{sti{13,arlb2(xr){{{store as lbd 2{22434
{{ldi{4,intv2{{{load integer two{22435
{{sti{13,ardm2(xr){{{store as dim 2{22436
{{mti{8,wb{{{get element count as integer{22437
{{sti{13,ardim(xr){{{store as dim 1{22438
{{zer{13,arpr2(xr){{{zero prototype field for now{22439
{{mov{13,arofs(xr){19,*arpr2{{set offset field (signal pass 2){22440
{{mov{8,wb{7,xr{{save arblk pointer{22441
{{add{7,xr{19,*arvl2{{point to first element location{22442
{{brn{6,gtar1{{{jump back to fill in elements{22443
{{ejc{{{{{22444
*      gtarr (continued)
*      here after filling in element values
{gtar7{mov{7,xr{8,wb{{restore arblk pointer{22450
{{mov{9,(xs){8,wb{{store as result{22451
*      now we need the array prototype which is of the form nn,2
*      this is obtained by building the string for nn02 and
*      changing the zero to a comma before storing it.
{{ldi{13,ardim(xr){{{get number of elements (nn){22457
{{mli{4,intvh{{{multiply by 100{22458
{{adi{4,intv2{{{add 2 (nn02){22459
{{jsr{6,icbld{{{build integer{22460
{{mov{11,-(xs){7,xr{{store ptr for gtstg{22461
{{jsr{6,gtstg{{{convert to string{22462
{{ppm{{{{convert fail is impossible{22463
{{mov{7,xl{7,xr{{copy string pointer{22464
{{mov{7,xr{10,(xs)+{{reload arblk pointer{22465
{{mov{13,arpr2(xr){7,xl{{store prototype ptr (nn02){22466
{{sub{8,wa{18,=num02{{adjust length to point to zero{22467
{{psc{7,xl{8,wa{{point to zero{22468
{{mov{8,wb{18,=ch_cm{{load a comma{22469
{{sch{8,wb{9,(xl){{store a comma over the zero{22470
{{csc{7,xl{{{complete store characters{22471
*      normal return
{gtar8{exi{{{{return to caller{22475
*      null table non-conversion return
{gtar9{mov{7,xr{10,(xs)+{{restore stack for conv err (sgd02){22479
{{exi{1,1{{{return{22480
*      impossible conversion return
{gta9a{exi{1,2{{{return{22484
*      array size too large
{gta9b{erb{1,260{26,conversion array size exceeds maximum permitted{{{22488
{{enp{{{{procedure gtarr{22489
{{ejc{{{{{22490
*      gtcod -- convert to code
*      (xr)                  object to be converted
*      jsr  gtcod            call to convert to code
*      ppm  loc              transfer loc if convert impossible
*      (xr)                  pointer to resulting cdblk
*      (xl,wa,wb,wc,ra)      destroyed
*      if a spitbol error occurs during compilation or pre-
*      evaluation, control is passed via error section to exfal
*      without returning to this routine.
{gtcod{prc{25,e{1,1{{entry point{22504
{{beq{9,(xr){22,=b_cds{6,gtcd1{jump if already code{22505
{{beq{9,(xr){22,=b_cdc{6,gtcd1{jump if already code{22506
*      here we must generate a cdblk by compilation
{{mov{11,-(xs){7,xr{{stack argument for gtstg{22510
{{jsr{6,gtstg{{{convert argument to string{22511
{{ppm{6,gtcd2{{{jump if non-convertible{22512
{{mov{3,gtcef{3,flptr{{save fail ptr in case of error{22513
{{mov{3,r_gtc{3,r_cod{{also save code ptr{22514
{{mov{3,r_cim{7,xr{{else set image pointer{22515
{{mov{3,scnil{8,wa{{set image length{22516
{{zer{3,scnpt{{{set scan pointer{22517
{{mov{3,stage{18,=stgxc{{set stage for execute compile{22518
{{mov{3,lstsn{3,cmpsn{{in case listr called{22519
{{icv{3,cmpln{{{bump line number{22521
{{jsr{6,cmpil{{{compile string{22523
{{mov{3,stage{18,=stgxt{{reset stage for execute time{22524
{{zer{3,r_cim{{{clear image{22525
*      merge here if no convert required
{gtcd1{exi{{{{give normal gtcod return{22529
*      here if unconvertible
{gtcd2{exi{1,1{{{give error return{22533
{{enp{{{{end procedure gtcod{22534
{{ejc{{{{{22535
*      gtexp -- convert to expression
*      (wb)                  0 if by value, 1 if by name
*      (xr)                  input value to be converted
*      jsr  gtexp            call to convert to expression
*      ppm  loc              transfer loc if convert impossible
*      (xr)                  pointer to result exblk or seblk
*      (xl,wa,wb,wc,ra)      destroyed
*      if a spitbol error occurs during compilation or pre-
*      evaluation, control is passed via error section to exfal
*      without returning to this routine.
{gtexp{prc{25,e{1,1{{entry point{22552
{{blo{9,(xr){22,=b_e__{6,gtex1{jump if already an expression{22553
{{mov{11,-(xs){7,xr{{store argument for gtstg{22554
{{jsr{6,gtstg{{{convert argument to string{22555
{{ppm{6,gtex2{{{jump if unconvertible{22556
*      check the last character of the string for colon or
*      semicolon.  these characters can legitimately end an
*      expression in open code, so expan will not detect them
*      as errors, but they are invalid as terminators for a
*      string that is being converted to expression form.
{{mov{7,xl{7,xr{{copy input string pointer{22564
{{plc{7,xl{8,wa{{point one past the string end{22565
{{lch{7,xl{11,-(xl){{fetch the last character{22566
{{beq{7,xl{18,=ch_cl{6,gtex2{error if it is a semicolon{22567
{{beq{7,xl{18,=ch_sm{6,gtex2{or if it is a colon{22568
*      here we convert a string by compilation
{{mov{3,r_cim{7,xr{{set input image pointer{22572
{{zer{3,scnpt{{{set scan pointer{22573
{{mov{3,scnil{8,wa{{set input image length{22574
{{mov{11,-(xs){8,wb{{save value/name flag{22576
{{zer{8,wb{{{set code for normal scan{22578
{{mov{3,gtcef{3,flptr{{save fail ptr in case of error{22579
{{mov{3,r_gtc{3,r_cod{{also save code ptr{22580
{{mov{3,stage{18,=stgev{{adjust stage for compile{22581
{{mov{3,scntp{18,=t_uok{{indicate unary operator acceptable{22582
{{jsr{6,expan{{{build tree for expression{22583
{{zer{3,scnrs{{{reset rescan flag{22584
{{mov{8,wa{10,(xs)+{{restore value/name flag{22586
{{bne{3,scnpt{3,scnil{6,gtex2{error if not end of image{22588
{{zer{8,wb{{{set ok value for cdgex call{22589
{{mov{7,xl{7,xr{{copy tree pointer{22590
{{jsr{6,cdgex{{{build expression block{22591
{{zer{3,r_cim{{{clear pointer{22592
{{mov{3,stage{18,=stgxt{{restore stage for execute time{22593
*      merge here if no conversion required
{gtex1{exi{{{{return to gtexp caller{22597
*      here if unconvertible
{gtex2{exi{1,1{{{take error exit{22601
{{enp{{{{end procedure gtexp{22602
{{ejc{{{{{22603
*      gtint -- get integer value
*      gtint is passed an object and returns an integer after
*      performing any necessary conversions.
*      (xr)                  value to be converted
*      jsr  gtint            call to convert to integer
*      ppm  loc              transfer loc for convert impossible
*      (xr)                  resulting integer
*      (wc,ra)               destroyed
*      (wa,wb)               destroyed (only on conversion err)
*      (xr)                  unchanged (on convert error)
{gtint{prc{25,e{1,1{{entry point{22618
{{beq{9,(xr){22,=b_icl{6,gtin2{jump if already an integer{22619
{{mov{3,gtina{8,wa{{else save wa{22620
{{mov{3,gtinb{8,wb{{save wb{22621
{{jsr{6,gtnum{{{convert to numeric{22622
{{ppm{6,gtin3{{{jump if unconvertible{22623
{{beq{8,wa{22,=b_icl{6,gtin1{jump if integer{22626
*      here we convert a real to integer
{{ldr{13,rcval(xr){{{load real value{22630
{{rti{6,gtin3{{{convert to integer (err if ovflow){22631
{{jsr{6,icbld{{{if ok build icblk{22632
*      here after successful conversion to integer
{gtin1{mov{8,wa{3,gtina{{restore wa{22637
{{mov{8,wb{3,gtinb{{restore wb{22638
*      common exit point
{gtin2{exi{{{{return to gtint caller{22642
*      here on conversion error
{gtin3{exi{1,1{{{take convert error exit{22646
{{enp{{{{end procedure gtint{22647
{{ejc{{{{{22648
*      gtnum -- get numeric value
*      gtnum is given an object and returns either an integer
*      or a real, performing any necessary conversions.
*      (xr)                  object to be converted
*      jsr  gtnum            call to convert to numeric
*      ppm  loc              transfer loc if convert impossible
*      (xr)                  pointer to result (int or real)
*      (wa)                  first word of result block
*      (wb,wc,ra)            destroyed
*      (xr)                  unchanged (on convert error)
{gtnum{prc{25,e{1,1{{entry point{22663
{{mov{8,wa{9,(xr){{load first word of block{22664
{{beq{8,wa{22,=b_icl{6,gtn34{jump if integer (no conversion){22665
{{beq{8,wa{22,=b_rcl{6,gtn34{jump if real (no conversion){22668
*      at this point the only possibility is to convert a string
*      to an integer or real as appropriate.
{{mov{11,-(xs){7,xr{{stack argument in case convert err{22674
{{mov{11,-(xs){7,xr{{stack argument for gtstg{22675
{{jsr{6,gtstg{{{convert argument to string{22677
{{ppm{6,gtn36{{{jump if unconvertible{22681
*      initialize numeric conversion
{{ldi{4,intv0{{{initialize integer result to zero{22685
{{bze{8,wa{6,gtn32{{jump to exit with zero if null{22686
{{lct{8,wa{8,wa{{set bct counter for following loops{22687
{{zer{3,gtnnf{{{tentatively indicate result +{22688
{{sti{3,gtnex{{{initialise exponent to zero{22691
{{zer{3,gtnsc{{{zero scale in case real{22692
{{zer{3,gtndf{{{reset flag for dec point found{22693
{{zer{3,gtnrd{{{reset flag for digits found{22694
{{ldr{4,reav0{{{zero real accum in case real{22695
{{plc{7,xr{{{point to argument characters{22697
*      merge back here after ignoring leading blank
{gtn01{lch{8,wb{10,(xr)+{{load first character{22701
{{blt{8,wb{18,=ch_d0{6,gtn02{jump if not digit{22702
{{ble{8,wb{18,=ch_d9{6,gtn06{jump if first char is a digit{22703
{{ejc{{{{{22704
*      gtnum (continued)
*      here if first digit is non-digit
{gtn02{bne{8,wb{18,=ch_bl{6,gtn03{jump if non-blank{22710
{gtna2{bct{8,wa{6,gtn01{{else decr count and loop back{22711
{{brn{6,gtn07{{{jump to return zero if all blanks{22712
*      here for first character non-blank, non-digit
{gtn03{beq{8,wb{18,=ch_pl{6,gtn04{jump if plus sign{22716
{{beq{8,wb{18,=ch_ht{6,gtna2{horizontal tab equiv to blank{22718
{{bne{8,wb{18,=ch_mn{6,gtn12{jump if not minus (may be real){22726
{{mnz{3,gtnnf{{{if minus sign, set negative flag{22728
*      merge here after processing sign
{gtn04{bct{8,wa{6,gtn05{{jump if chars left{22732
{{brn{6,gtn36{{{else error{22733
*      loop to fetch characters of an integer
{gtn05{lch{8,wb{10,(xr)+{{load next character{22737
{{blt{8,wb{18,=ch_d0{6,gtn08{jump if not a digit{22738
{{bgt{8,wb{18,=ch_d9{6,gtn08{jump if not a digit{22739
*      merge here for first digit
{gtn06{sti{3,gtnsi{{{save current value{22743
{{cvm{6,gtn35{{{current*10-(new dig) jump if ovflow{22747
{{mnz{3,gtnrd{{{set digit read flag{22748
{{bct{8,wa{6,gtn05{{else loop back if more chars{22750
*      here to exit with converted integer value
{gtn07{bnz{3,gtnnf{6,gtn32{{jump if negative (all set){22754
{{ngi{{{{else negate{22755
{{ino{6,gtn32{{{jump if no overflow{22756
{{brn{6,gtn36{{{else signal error{22757
{{ejc{{{{{22758
*      gtnum (continued)
*      here for a non-digit character while attempting to
*      convert an integer, check for trailing blanks or real.
{gtn08{beq{8,wb{18,=ch_bl{6,gtna9{jump if a blank{22765
{{beq{8,wb{18,=ch_ht{6,gtna9{jump if horizontal tab{22767
{{itr{{{{else convert integer to real{22775
{{ngr{{{{negate to get positive value{22776
{{brn{6,gtn12{{{jump to try for real{22777
*      here we scan out blanks to end of string
{gtn09{lch{8,wb{10,(xr)+{{get next char{22782
{{beq{8,wb{18,=ch_ht{6,gtna9{jump if horizontal tab{22784
{{bne{8,wb{18,=ch_bl{6,gtn36{error if non-blank{22789
{gtna9{bct{8,wa{6,gtn09{{loop back if more chars to check{22790
{{brn{6,gtn07{{{return integer if all blanks{22791
*      loop to collect mantissa of real
{gtn10{lch{8,wb{10,(xr)+{{load next character{22797
{{blt{8,wb{18,=ch_d0{6,gtn12{jump if non-numeric{22798
{{bgt{8,wb{18,=ch_d9{6,gtn12{jump if non-numeric{22799
*      merge here to collect first real digit
{gtn11{sub{8,wb{18,=ch_d0{{convert digit to number{22803
{{mlr{4,reavt{{{multiply real by 10.0{22804
{{rov{6,gtn36{{{convert error if overflow{22805
{{str{3,gtnsr{{{save result{22806
{{mti{8,wb{{{get new digit as integer{22807
{{itr{{{{convert new digit to real{22808
{{adr{3,gtnsr{{{add to get new total{22809
{{add{3,gtnsc{3,gtndf{{increment scale if after dec point{22810
{{mnz{3,gtnrd{{{set digit found flag{22811
{{bct{8,wa{6,gtn10{{loop back if more chars{22812
{{brn{6,gtn22{{{else jump to scale{22813
{{ejc{{{{{22814
*      gtnum (continued)
*      here if non-digit found while collecting a real
{gtn12{bne{8,wb{18,=ch_dt{6,gtn13{jump if not dec point{22820
{{bnz{3,gtndf{6,gtn36{{if dec point, error if one already{22821
{{mov{3,gtndf{18,=num01{{else set flag for dec point{22822
{{bct{8,wa{6,gtn10{{loop back if more chars{22823
{{brn{6,gtn22{{{else jump to scale{22824
*      here if not decimal point
{gtn13{beq{8,wb{18,=ch_le{6,gtn15{jump if e for exponent{22828
{{beq{8,wb{18,=ch_ld{6,gtn15{jump if d for exponent{22829
*      here check for trailing blanks
{gtn14{beq{8,wb{18,=ch_bl{6,gtnb4{jump if blank{22837
{{beq{8,wb{18,=ch_ht{6,gtnb4{jump if horizontal tab{22839
{{brn{6,gtn36{{{error if non-blank{22844
{gtnb4{lch{8,wb{10,(xr)+{{get next character{22846
{{bct{8,wa{6,gtn14{{loop back to check if more{22847
{{brn{6,gtn22{{{else jump to scale{22848
*      here to read and process an exponent
{gtn15{zer{3,gtnes{{{set exponent sign positive{22852
{{ldi{4,intv0{{{initialize exponent to zero{22853
{{mnz{3,gtndf{{{reset no dec point indication{22854
{{bct{8,wa{6,gtn16{{jump skipping past e or d{22855
{{brn{6,gtn36{{{error if null exponent{22856
*      check for exponent sign
{gtn16{lch{8,wb{10,(xr)+{{load first exponent character{22860
{{beq{8,wb{18,=ch_pl{6,gtn17{jump if plus sign{22861
{{bne{8,wb{18,=ch_mn{6,gtn19{else jump if not minus sign{22862
{{mnz{3,gtnes{{{set sign negative if minus sign{22863
*      merge here after processing exponent sign
{gtn17{bct{8,wa{6,gtn18{{jump if chars left{22867
{{brn{6,gtn36{{{else error{22868
*      loop to convert exponent digits
{gtn18{lch{8,wb{10,(xr)+{{load next character{22872
{{ejc{{{{{22873
*      gtnum (continued)
*      merge here for first exponent digit
{gtn19{blt{8,wb{18,=ch_d0{6,gtn20{jump if not digit{22879
{{bgt{8,wb{18,=ch_d9{6,gtn20{jump if not digit{22880
{{cvm{6,gtn36{{{else current*10, subtract new digit{22881
{{bct{8,wa{6,gtn18{{loop back if more chars{22882
{{brn{6,gtn21{{{jump if exponent field is exhausted{22883
*      here to check for trailing blanks after exponent
{gtn20{beq{8,wb{18,=ch_bl{6,gtnc0{jump if blank{22887
{{beq{8,wb{18,=ch_ht{6,gtnc0{jump if horizontal tab{22889
{{brn{6,gtn36{{{error if non-blank{22894
{gtnc0{lch{8,wb{10,(xr)+{{get next character{22896
{{bct{8,wa{6,gtn20{{loop back till all blanks scanned{22897
*      merge here after collecting exponent
{gtn21{sti{3,gtnex{{{save collected exponent{22901
{{bnz{3,gtnes{6,gtn22{{jump if it was negative{22902
{{ngi{{{{else complement{22903
{{iov{6,gtn36{{{error if overflow{22904
{{sti{3,gtnex{{{and store positive exponent{22905
*      merge here with exponent (0 if none given)
{gtn22{bze{3,gtnrd{6,gtn36{{error if not digits collected{22909
{{bze{3,gtndf{6,gtn36{{error if no exponent or dec point{22910
{{mti{3,gtnsc{{{else load scale as integer{22911
{{sbi{3,gtnex{{{subtract exponent{22912
{{iov{6,gtn36{{{error if overflow{22913
{{ilt{6,gtn26{{{jump if we must scale up{22914
*      here we have a negative exponent, so scale down
{{mfi{8,wa{6,gtn36{{load scale factor, err if ovflow{22918
*      loop to scale down in steps of 10**10
{gtn23{ble{8,wa{18,=num10{6,gtn24{jump if 10 or less to go{22922
{{dvr{4,reatt{{{else divide by 10**10{22923
{{sub{8,wa{18,=num10{{decrement scale{22924
{{brn{6,gtn23{{{and loop back{22925
{{ejc{{{{{22926
*      gtnum (continued)
*      here scale rest of way from powers of ten table
{gtn24{bze{8,wa{6,gtn30{{jump if scaled{22932
{{lct{8,wb{18,=cfp_r{{else get indexing factor{22933
{{mov{7,xr{21,=reav1{{point to powers of ten table{22934
{{wtb{8,wa{{{convert remaining scale to byte ofs{22935
*      loop to point to powers of ten table entry
{gtn25{add{7,xr{8,wa{{bump pointer{22939
{{bct{8,wb{6,gtn25{{once for each value word{22940
{{dvr{9,(xr){{{scale down as required{22941
{{brn{6,gtn30{{{and jump{22942
*      come here to scale result up (positive exponent)
{gtn26{ngi{{{{get absolute value of exponent{22946
{{iov{6,gtn36{{{error if overflow{22947
{{mfi{8,wa{6,gtn36{{acquire scale, error if ovflow{22948
*      loop to scale up in steps of 10**10
{gtn27{ble{8,wa{18,=num10{6,gtn28{jump if 10 or less to go{22952
{{mlr{4,reatt{{{else multiply by 10**10{22953
{{rov{6,gtn36{{{error if overflow{22954
{{sub{8,wa{18,=num10{{else decrement scale{22955
{{brn{6,gtn27{{{and loop back{22956
*      here to scale up rest of way with table
{gtn28{bze{8,wa{6,gtn30{{jump if scaled{22960
{{lct{8,wb{18,=cfp_r{{else get indexing factor{22961
{{mov{7,xr{21,=reav1{{point to powers of ten table{22962
{{wtb{8,wa{{{convert remaining scale to byte ofs{22963
*      loop to point to proper entry in powers of ten table
{gtn29{add{7,xr{8,wa{{bump pointer{22967
{{bct{8,wb{6,gtn29{{once for each word in value{22968
{{mlr{9,(xr){{{scale up{22969
{{rov{6,gtn36{{{error if overflow{22970
{{ejc{{{{{22971
*      gtnum (continued)
*      here with real value scaled and ready except for sign
{gtn30{bze{3,gtnnf{6,gtn31{{jump if positive{22977
{{ngr{{{{else negate{22978
*      here with properly signed real value in (ra)
{gtn31{jsr{6,rcbld{{{build real block{22982
{{brn{6,gtn33{{{merge to exit{22983
*      here with properly signed integer value in (ia)
{gtn32{jsr{6,icbld{{{build icblk{22988
*      real merges here
{gtn33{mov{8,wa{9,(xr){{load first word of result block{22992
{{ica{7,xs{{{pop argument off stack{22993
*      common exit point
{gtn34{exi{{{{return to gtnum caller{22997
*      come here if overflow occurs during collection of integer
*      have to restore wb which cvm may have destroyed.
{gtn35{lch{8,wb{11,-(xr){{reload current character{23004
{{lch{8,wb{10,(xr)+{{bump character pointer{23005
{{ldi{3,gtnsi{{{reload integer so far{23006
{{itr{{{{convert to real{23007
{{ngr{{{{make value positive{23008
{{brn{6,gtn11{{{merge with real circuit{23009
*      here for unconvertible to string or conversion error
{gtn36{mov{7,xr{10,(xs)+{{reload original argument{23014
{{exi{1,1{{{take convert-error exit{23015
{{enp{{{{end procedure gtnum{23016
{{ejc{{{{{23017
*      gtnvr -- convert to natural variable
*      gtnvr locates a variable block (vrblk) given either an
*      appropriate name (nmblk) or a non-null string (scblk).
*      (xr)                  argument
*      jsr  gtnvr            call to convert to natural variable
*      ppm  loc              transfer loc if convert impossible
*      (xr)                  pointer to vrblk
*      (wa,wb)               destroyed (conversion error only)
*      (wc)                  destroyed
{gtnvr{prc{25,e{1,1{{entry point{23031
*z-
{{bne{9,(xr){22,=b_nml{6,gnv02{jump if not name{23033
{{mov{7,xr{13,nmbas(xr){{else load name base if name{23034
{{blo{7,xr{3,state{6,gnv07{skip if vrblk (in static region){23035
*      common error exit
{gnv01{exi{1,1{{{take convert-error exit{23039
*      here if not name
{gnv02{mov{3,gnvsa{8,wa{{save wa{23043
{{mov{3,gnvsb{8,wb{{save wb{23044
{{mov{11,-(xs){7,xr{{stack argument for gtstg{23045
{{jsr{6,gtstg{{{convert argument to string{23046
{{ppm{6,gnv01{{{jump if conversion error{23047
{{bze{8,wa{6,gnv01{{null string is an error{23048
{{mov{11,-(xs){7,xl{{save xl{23052
{{mov{11,-(xs){7,xr{{stack string ptr for later{23053
{{mov{8,wb{7,xr{{copy string pointer{23054
{{add{8,wb{19,*schar{{point to characters of string{23055
{{mov{3,gnvst{8,wb{{save pointer to characters{23056
{{mov{8,wb{8,wa{{copy length{23057
{{ctw{8,wb{1,0{{get number of words in name{23058
{{mov{3,gnvnw{8,wb{{save for later{23059
{{jsr{6,hashs{{{compute hash index for string{23060
{{rmi{3,hshnb{{{compute hash offset by taking mod{23061
{{mfi{8,wc{{{get as offset{23062
{{wtb{8,wc{{{convert offset to bytes{23063
{{add{8,wc{3,hshtb{{point to proper hash chain{23064
{{sub{8,wc{19,*vrnxt{{subtract offset to merge into loop{23065
{{ejc{{{{{23066
*      gtnvr (continued)
*      loop to search hash chain
{gnv03{mov{7,xl{8,wc{{copy hash chain pointer{23072
{{mov{7,xl{13,vrnxt(xl){{point to next vrblk on chain{23073
{{bze{7,xl{6,gnv08{{jump if end of chain{23074
{{mov{8,wc{7,xl{{save pointer to this vrblk{23075
{{bnz{13,vrlen(xl){6,gnv04{{jump if not system variable{23076
{{mov{7,xl{13,vrsvp(xl){{else point to svblk{23077
{{sub{7,xl{19,*vrsof{{adjust offset for merge{23078
*      merge here with string ptr (like vrblk) in xl
{gnv04{bne{8,wa{13,vrlen(xl){6,gnv03{back for next vrblk if lengths ne{23082
{{add{7,xl{19,*vrchs{{else point to chars of chain entry{23083
{{lct{8,wb{3,gnvnw{{get word counter to control loop{23084
{{mov{7,xr{3,gnvst{{point to chars of new name{23085
*      loop to compare characters of the two names
{gnv05{cne{9,(xr){9,(xl){6,gnv03{jump if no match for next vrblk{23089
{{ica{7,xr{{{bump new name pointer{23090
{{ica{7,xl{{{bump vrblk in chain name pointer{23091
{{bct{8,wb{6,gnv05{{else loop till all compared{23092
{{mov{7,xr{8,wc{{we have found a match, get vrblk{23093
*      exit point after finding vrblk or building new one
{gnv06{mov{8,wa{3,gnvsa{{restore wa{23097
{{mov{8,wb{3,gnvsb{{restore wb{23098
{{ica{7,xs{{{pop string pointer{23099
{{mov{7,xl{10,(xs)+{{restore xl{23100
*      common exit point
{gnv07{exi{{{{return to gtnvr caller{23104
*      not found, prepare to search system variable table
{gnv08{zer{7,xr{{{clear garbage xr pointer{23108
{{mov{3,gnvhe{8,wc{{save ptr to end of hash chain{23109
{{bgt{8,wa{18,=num09{6,gnv14{cannot be system var if length gt 9{23110
{{mov{7,xl{8,wa{{else copy length{23111
{{wtb{7,xl{{{convert to byte offset{23112
{{mov{7,xl{14,vsrch(xl){{point to first svblk of this length{23113
{{ejc{{{{{23114
*      gtnvr (continued)
*      loop to search entries in standard variable table
{gnv09{mov{3,gnvsp{7,xl{{save table pointer{23120
{{mov{8,wc{10,(xl)+{{load svbit bit string{23121
{{mov{8,wb{10,(xl)+{{load length from table entry{23122
{{bne{8,wa{8,wb{6,gnv14{jump if end of right length entries{23123
{{lct{8,wb{3,gnvnw{{get word counter to control loop{23124
{{mov{7,xr{3,gnvst{{point to chars of new name{23125
*      loop to check for matching names
{gnv10{cne{9,(xr){9,(xl){6,gnv11{jump if name mismatch{23129
{{ica{7,xr{{{else bump new name pointer{23130
{{ica{7,xl{{{bump svblk pointer{23131
{{bct{8,wb{6,gnv10{{else loop until all checked{23132
*      here we have a match in the standard variable table
{{zer{8,wc{{{set vrlen value zero{23136
{{mov{8,wa{19,*vrsi_{{set standard size{23137
{{brn{6,gnv15{{{jump to build vrblk{23138
*      here if no match with table entry in svblks table
{gnv11{ica{7,xl{{{bump past word of chars{23142
{{bct{8,wb{6,gnv11{{loop back if more to go{23143
{{rsh{8,wc{2,svnbt{{remove uninteresting bits{23144
*      loop to bump table ptr for each flagged word
{gnv12{mov{8,wb{4,bits1{{load bit to test{23148
{{anb{8,wb{8,wc{{test for word present{23149
{{zrb{8,wb{6,gnv13{{jump if not present{23150
{{ica{7,xl{{{else bump table pointer{23151
*      here after dealing with one word (one bit)
{gnv13{rsh{8,wc{1,1{{remove bit already processed{23155
{{nzb{8,wc{6,gnv12{{loop back if more bits to test{23156
{{brn{6,gnv09{{{else loop back for next svblk{23157
*      here if not system variable
{gnv14{mov{8,wc{8,wa{{copy vrlen value{23161
{{mov{8,wa{18,=vrchs{{load standard size -chars{23162
{{add{8,wa{3,gnvnw{{adjust for chars of name{23163
{{wtb{8,wa{{{convert length to bytes{23164
{{ejc{{{{{23165
*      gtnvr (continued)
*      merge here to build vrblk
{gnv15{jsr{6,alost{{{allocate space for vrblk (static){23171
{{mov{8,wb{7,xr{{save vrblk pointer{23172
{{mov{7,xl{21,=stnvr{{point to model variable block{23173
{{mov{8,wa{19,*vrlen{{set length of standard fields{23174
{{mvw{{{{set initial fields of new block{23175
{{mov{7,xl{3,gnvhe{{load pointer to end of hash chain{23176
{{mov{13,vrnxt(xl){8,wb{{add new block to end of chain{23177
{{mov{10,(xr)+{8,wc{{set vrlen field, bump ptr{23178
{{mov{8,wa{3,gnvnw{{get length in words{23179
{{wtb{8,wa{{{convert to length in bytes{23180
{{bze{8,wc{6,gnv16{{jump if system variable{23181
*      here for non-system variable -- set chars of name
{{mov{7,xl{9,(xs){{point back to string name{23185
{{add{7,xl{19,*schar{{point to chars of name{23186
{{mvw{{{{move characters into place{23187
{{mov{7,xr{8,wb{{restore vrblk pointer{23188
{{brn{6,gnv06{{{jump back to exit{23189
*      here for system variable case to fill in fields where
*      necessary from the fields present in the svblk.
{gnv16{mov{7,xl{3,gnvsp{{load pointer to svblk{23194
{{mov{9,(xr){7,xl{{set svblk ptr in vrblk{23195
{{mov{7,xr{8,wb{{restore vrblk pointer{23196
{{mov{8,wb{13,svbit(xl){{load bit indicators{23197
{{add{7,xl{19,*svchs{{point to characters of name{23198
{{add{7,xl{8,wa{{point past characters{23199
*      skip past keyword number (svknm) if present
{{mov{8,wc{4,btknm{{load test bit{23203
{{anb{8,wc{8,wb{{and to test{23204
{{zrb{8,wc{6,gnv17{{jump if no keyword number{23205
{{ica{7,xl{{{else bump pointer{23206
{{ejc{{{{{23207
*      gtnvr (continued)
*      here test for function (svfnc and svnar)
{gnv17{mov{8,wc{4,btfnc{{get test bit{23213
{{anb{8,wc{8,wb{{and to test{23214
{{zrb{8,wc{6,gnv18{{skip if no system function{23215
{{mov{13,vrfnc(xr){7,xl{{else point vrfnc to svfnc field{23216
{{add{7,xl{19,*num02{{and bump past svfnc, svnar fields{23217
*      now test for label (svlbl)
{gnv18{mov{8,wc{4,btlbl{{get test bit{23221
{{anb{8,wc{8,wb{{and to test{23222
{{zrb{8,wc{6,gnv19{{jump if bit is off (no system labl){23223
{{mov{13,vrlbl(xr){7,xl{{else point vrlbl to svlbl field{23224
{{ica{7,xl{{{bump past svlbl field{23225
*      now test for value (svval)
{gnv19{mov{8,wc{4,btval{{load test bit{23229
{{anb{8,wc{8,wb{{and to test{23230
{{zrb{8,wc{6,gnv06{{all done if no value{23231
{{mov{13,vrval(xr){9,(xl){{else set initial value{23232
{{mov{13,vrsto(xr){22,=b_vre{{set error store access{23233
{{brn{6,gnv06{{{merge back to exit to caller{23234
{{enp{{{{end procedure gtnvr{23235
{{ejc{{{{{23236
*      gtpat -- get pattern
*      gtpat is passed an object in (xr) and returns a
*      pattern after performing any necessary conversions
*      (xr)                  input argument
*      jsr  gtpat            call to convert to pattern
*      ppm  loc              transfer loc if convert impossible
*      (xr)                  resulting pattern
*      (wa)                  destroyed
*      (wb)                  destroyed (only on convert error)
*      (xr)                  unchanged (only on convert error)
{gtpat{prc{25,e{1,1{{entry point{23251
*z+
{{bhi{9,(xr){22,=p_aaa{6,gtpt5{jump if pattern already{23253
*      here if not pattern, try for string
{{mov{3,gtpsb{8,wb{{save wb{23257
{{mov{11,-(xs){7,xr{{stack argument for gtstg{23258
{{jsr{6,gtstg{{{convert argument to string{23259
{{ppm{6,gtpt2{{{jump if impossible{23260
*      here we have a string
{{bnz{8,wa{6,gtpt1{{jump if non-null{23264
*      here for null string. generate pointer to null pattern.
{{mov{7,xr{21,=ndnth{{point to nothen node{23268
{{brn{6,gtpt4{{{jump to exit{23269
{{ejc{{{{{23270
*      gtpat (continued)
*      here for non-null string
{gtpt1{mov{8,wb{22,=p_str{{load pcode for multi-char string{23276
{{bne{8,wa{18,=num01{6,gtpt3{jump if multi-char string{23277
*      here for one character string, share one character any
{{plc{7,xr{{{point to character{23281
{{lch{8,wa{9,(xr){{load character{23282
{{mov{7,xr{8,wa{{set as parm1{23283
{{mov{8,wb{22,=p_ans{{point to pcode for 1-char any{23284
{{brn{6,gtpt3{{{jump to build node{23285
*      here if argument is not convertible to string
{gtpt2{mov{8,wb{22,=p_exa{{set pcode for expression in case{23289
{{blo{9,(xr){22,=b_e__{6,gtpt3{jump to build node if expression{23290
*      here we have an error (conversion impossible)
{{exi{1,1{{{take convert error exit{23294
*      merge here to build node for string or expression
{gtpt3{jsr{6,pbild{{{call routine to build pattern node{23298
*      common exit after successful conversion
{gtpt4{mov{8,wb{3,gtpsb{{restore wb{23302
*      merge here to exit if no conversion required
{gtpt5{exi{{{{return to gtpat caller{23306
{{enp{{{{end procedure gtpat{23307
{{ejc{{{{{23310
*      gtrea -- get real value
*      gtrea is passed an object and returns a real value
*      performing any necessary conversions.
*      (xr)                  object to be converted
*      jsr  gtrea            call to convert object to real
*      ppm  loc              transfer loc if convert impossible
*      (xr)                  pointer to resulting real
*      (wa,wb,wc,ra)         destroyed
*      (xr)                  unchanged (convert error only)
{gtrea{prc{25,e{1,1{{entry point{23324
{{mov{8,wa{9,(xr){{get first word of block{23325
{{beq{8,wa{22,=b_rcl{6,gtre2{jump if real{23326
{{jsr{6,gtnum{{{else convert argument to numeric{23327
{{ppm{6,gtre3{{{jump if unconvertible{23328
{{beq{8,wa{22,=b_rcl{6,gtre2{jump if real was returned{23329
*      here for case of an integer to convert to real
{gtre1{ldi{13,icval(xr){{{load integer{23333
{{itr{{{{convert to real{23334
{{jsr{6,rcbld{{{build rcblk{23335
*      exit with real
{gtre2{exi{{{{return to gtrea caller{23339
*      here on conversion error
{gtre3{exi{1,1{{{take convert error exit{23343
{{enp{{{{end procedure gtrea{23344
{{ejc{{{{{23346
*      gtsmi -- get small integer
*      gtsmi is passed a snobol object and returns an address
*      integer in the range (0 le n le dnamb). such a value can
*      only be derived from an integer in the appropriate range.
*      small integers never appear as snobol values. however,
*      they are used internally for a variety of purposes.
*      -(xs)                 argument to convert (on stack)
*      jsr  gtsmi            call to convert to small integer
*      ppm  loc              transfer loc for not integer
*      ppm  loc              transfer loc for lt 0, gt dnamb
*      (xr,wc)               resulting small int (two copies)
*      (xs)                  popped
*      (ra)                  destroyed
*      (wa,wb)               destroyed (on convert error only)
*      (xr)                  input arg (convert error only)
{gtsmi{prc{25,n{1,2{{entry point{23366
{{mov{7,xr{10,(xs)+{{load argument{23367
{{beq{9,(xr){22,=b_icl{6,gtsm1{skip if already an integer{23368
*      here if not an integer
{{jsr{6,gtint{{{convert argument to integer{23372
{{ppm{6,gtsm2{{{jump if convert is impossible{23373
*      merge here with integer
{gtsm1{ldi{13,icval(xr){{{load integer value{23377
{{mfi{8,wc{6,gtsm3{{move as one word, jump if ovflow{23378
{{bgt{8,wc{3,mxlen{6,gtsm3{or if too large{23379
{{mov{7,xr{8,wc{{copy result to xr{23380
{{exi{{{{return to gtsmi caller{23381
*      here if unconvertible to integer
{gtsm2{exi{1,1{{{take non-integer error exit{23385
*      here if out of range
{gtsm3{exi{1,2{{{take out-of-range error exit{23389
{{enp{{{{end procedure gtsmi{23390
{{ejc{{{{{23391
*      gtstg -- get string
*      gtstg is passed an object and returns a string with
*      any necessary conversions performed.
*      -(xs)                 input argument (on stack)
*      jsr  gtstg            call to convert to string
*      ppm  loc              transfer loc if convert impossible
*      (xr)                  pointer to resulting string
*      (wa)                  length of string in characters
*      (xs)                  popped
*      (ra)                  destroyed
*      (xr)                  input arg (convert error only)
{gtstg{prc{25,n{1,1{{entry point{23457
{{mov{7,xr{10,(xs)+{{load argument, pop stack{23458
{{beq{9,(xr){22,=b_scl{6,gts30{jump if already a string{23459
*      here if not a string already
{gts01{mov{11,-(xs){7,xr{{restack argument in case error{23463
{{mov{11,-(xs){7,xl{{save xl{23464
{{mov{3,gtsvb{8,wb{{save wb{23465
{{mov{3,gtsvc{8,wc{{save wc{23466
{{mov{8,wa{9,(xr){{load first word of block{23467
{{beq{8,wa{22,=b_icl{6,gts05{jump to convert integer{23468
{{beq{8,wa{22,=b_rcl{6,gts10{jump to convert real{23471
{{beq{8,wa{22,=b_nml{6,gts03{jump to convert name{23473
*      here on conversion error
{gts02{mov{7,xl{10,(xs)+{{restore xl{23481
{{mov{7,xr{10,(xs)+{{reload input argument{23482
{{exi{1,1{{{take convert error exit{23483
{{ejc{{{{{23484
*      gtstg (continued)
*      here to convert a name (only possible if natural var)
{gts03{mov{7,xl{13,nmbas(xr){{load name base{23490
{{bhi{7,xl{3,state{6,gts02{error if not natural var (static){23491
{{add{7,xl{19,*vrsof{{else point to possible string name{23492
{{mov{8,wa{13,sclen(xl){{load length{23493
{{bnz{8,wa{6,gts04{{jump if not system variable{23494
{{mov{7,xl{13,vrsvo(xl){{else point to svblk{23495
{{mov{8,wa{13,svlen(xl){{and load name length{23496
*      merge here with string in xr, length in wa
{gts04{zer{8,wb{{{set offset to zero{23500
{{jsr{6,sbstr{{{use sbstr to copy string{23501
{{brn{6,gts29{{{jump to exit{23502
*      come here to convert an integer
{gts05{ldi{13,icval(xr){{{load integer value{23506
{{mov{3,gtssf{18,=num01{{set sign flag negative{23514
{{ilt{6,gts06{{{skip if integer is negative{23515
{{ngi{{{{else negate integer{23516
{{zer{3,gtssf{{{and reset negative flag{23517
{{ejc{{{{{23518
*      gtstg (continued)
*      here with sign flag set and sign forced negative as
*      required by the cvd instruction.
{gts06{mov{7,xr{3,gtswk{{point to result work area{23525
{{mov{8,wb{18,=nstmx{{initialize counter to max length{23526
{{psc{7,xr{8,wb{{prepare to store (right-left){23527
*      loop to convert digits into work area
{gts07{cvd{{{{convert one digit into wa{23531
{{sch{8,wa{11,-(xr){{store in work area{23532
{{dcv{8,wb{{{decrement counter{23533
{{ine{6,gts07{{{loop if more digits to go{23534
{{csc{7,xr{{{complete store characters{23535
*      merge here after converting integer or real into work
*      area. wb is set to nstmx - (number of chars in result).
{gts08{mov{8,wa{18,=nstmx{{get max number of characters{23541
{{sub{8,wa{8,wb{{compute length of result{23542
{{mov{7,xl{8,wa{{remember length for move later on{23543
{{add{8,wa{3,gtssf{{add one for negative sign if needed{23544
{{jsr{6,alocs{{{allocate string for result{23545
{{mov{8,wc{7,xr{{save result pointer for the moment{23546
{{psc{7,xr{{{point to chars of result block{23547
{{bze{3,gtssf{6,gts09{{skip if positive{23548
{{mov{8,wa{18,=ch_mn{{else load negative sign{23549
{{sch{8,wa{10,(xr)+{{and store it{23550
{{csc{7,xr{{{complete store characters{23551
*      here after dealing with sign
{gts09{mov{8,wa{7,xl{{recall length to move{23555
{{mov{7,xl{3,gtswk{{point to result work area{23556
{{plc{7,xl{8,wb{{point to first result character{23557
{{mvc{{{{move chars to result string{23558
{{mov{7,xr{8,wc{{restore result pointer{23559
{{brn{6,gts29{{{jump to exit{23562
{{ejc{{{{{23563
*      gtstg (continued)
*      here to convert a real
{gts10{ldr{13,rcval(xr){{{load real{23569
{{zer{3,gtssf{{{reset negative flag{23581
{{req{6,gts31{{{skip if zero{23582
{{rge{6,gts11{{{jump if real is positive{23583
{{mov{3,gtssf{18,=num01{{else set negative flag{23584
{{ngr{{{{and get absolute value of real{23585
*      now scale the real to the range (0.1 le x lt 1.0)
{gts11{ldi{4,intv0{{{initialize exponent to zero{23589
*      loop to scale up in steps of 10**10
{gts12{str{3,gtsrs{{{save real value{23593
{{sbr{4,reap1{{{subtract 0.1 to compare{23594
{{rge{6,gts13{{{jump if scale up not required{23595
{{ldr{3,gtsrs{{{else reload value{23596
{{mlr{4,reatt{{{multiply by 10**10{23597
{{sbi{4,intvt{{{decrement exponent by 10{23598
{{brn{6,gts12{{{loop back to test again{23599
*      test for scale down required
{gts13{ldr{3,gtsrs{{{reload value{23603
{{sbr{4,reav1{{{subtract 1.0{23604
{{rlt{6,gts17{{{jump if no scale down required{23605
{{ldr{3,gtsrs{{{else reload value{23606
*      loop to scale down in steps of 10**10
{gts14{sbr{4,reatt{{{subtract 10**10 to compare{23610
{{rlt{6,gts15{{{jump if large step not required{23611
{{ldr{3,gtsrs{{{else restore value{23612
{{dvr{4,reatt{{{divide by 10**10{23613
{{str{3,gtsrs{{{store new value{23614
{{adi{4,intvt{{{increment exponent by 10{23615
{{brn{6,gts14{{{loop back{23616
{{ejc{{{{{23617
*      gtstg (continued)
*      at this point we have (1.0 le x lt 10**10)
*      complete scaling with powers of ten table
{gts15{mov{7,xr{21,=reav1{{point to powers of ten table{23624
*      loop to locate correct entry in table
{gts16{ldr{3,gtsrs{{{reload value{23628
{{adi{4,intv1{{{increment exponent{23629
{{add{7,xr{19,*cfp_r{{point to next entry in table{23630
{{sbr{9,(xr){{{subtract it to compare{23631
{{rge{6,gts16{{{loop till we find a larger entry{23632
{{ldr{3,gtsrs{{{then reload the value{23633
{{dvr{9,(xr){{{and complete scaling{23634
{{str{3,gtsrs{{{store value{23635
*      we are now scaled, so round by adding 0.5 * 10**(-cfp_s)
{gts17{ldr{3,gtsrs{{{get value again{23639
{{adr{3,gtsrn{{{add rounding factor{23640
{{str{3,gtsrs{{{store result{23641
*      the rounding operation may have pushed us up past
*      1.0 again, so check one more time.
{{sbr{4,reav1{{{subtract 1.0 to compare{23646
{{rlt{6,gts18{{{skip if ok{23647
{{adi{4,intv1{{{else increment exponent{23648
{{ldr{3,gtsrs{{{reload value{23649
{{dvr{4,reavt{{{divide by 10.0 to rescale{23650
{{brn{6,gts19{{{jump to merge{23651
*      here if rounding did not muck up scaling
{gts18{ldr{3,gtsrs{{{reload rounded value{23655
{{ejc{{{{{23656
*      gtstg (continued)
*      now we have completed the scaling as follows
*      (ia)                  signed exponent
*      (ra)                  scaled real (absolute value)
*      if the exponent is negative or greater than cfp_s, then
*      we convert the number in the form.
*      (neg sign) 0 . (cpf_s digits) e (exp sign) (exp digits)
*      if the exponent is positive and less than or equal to
*      cfp_s, the number is converted in the form.
*      (neg sign) (exponent digits) . (cfp_s-exponent digits)
*      in both cases, the formats obtained from the above
*      rules are modified by deleting trailing zeros after the
*      decimal point. there are no leading zeros in the exponent
*      and the exponent sign is always present.
{gts19{mov{7,xl{18,=cfp_s{{set num dec digits = cfp_s{23680
{{mov{3,gtses{18,=ch_mn{{set exponent sign negative{23681
{{ilt{6,gts21{{{all set if exponent is negative{23682
{{mfi{8,wa{{{else fetch exponent{23683
{{ble{8,wa{18,=cfp_s{6,gts20{skip if we can use special format{23684
{{mti{8,wa{{{else restore exponent{23685
{{ngi{{{{set negative for cvd{23686
{{mov{3,gtses{18,=ch_pl{{set plus sign for exponent sign{23687
{{brn{6,gts21{{{jump to generate exponent{23688
*      here if we can use the format without an exponent
{gts20{sub{7,xl{8,wa{{compute digits after decimal point{23692
{{ldi{4,intv0{{{reset exponent to zero{23693
{{ejc{{{{{23694
*      gtstg (continued)
*      merge here as follows
*      (ia)                  exponent absolute value
*      gtses                 character for exponent sign
*      (ra)                  positive fraction
*      (xl)                  number of digits after dec point
{gts21{mov{7,xr{3,gtswk{{point to work area{23705
{{mov{8,wb{18,=nstmx{{set character ctr to max length{23706
{{psc{7,xr{8,wb{{prepare to store (right to left){23707
{{ieq{6,gts23{{{skip exponent if it is zero{23708
*      loop to generate digits of exponent
{gts22{cvd{{{{convert a digit into wa{23712
{{sch{8,wa{11,-(xr){{store in work area{23713
{{dcv{8,wb{{{decrement counter{23714
{{ine{6,gts22{{{loop back if more digits to go{23715
*      here generate exponent sign and e
{{mov{8,wa{3,gtses{{load exponent sign{23719
{{sch{8,wa{11,-(xr){{store in work area{23720
{{mov{8,wa{18,=ch_le{{get character letter e{23721
{{sch{8,wa{11,-(xr){{store in work area{23722
{{sub{8,wb{18,=num02{{decrement counter for sign and e{23723
*      here to generate the fraction
{gts23{mlr{3,gtssc{{{convert real to integer (10**cfp_s){23727
{{rti{{{{get integer (overflow impossible){23728
{{ngi{{{{negate as required by cvd{23729
*      loop to suppress trailing zeros
{gts24{bze{7,xl{6,gts27{{jump if no digits left to do{23733
{{cvd{{{{else convert one digit{23734
{{bne{8,wa{18,=ch_d0{6,gts26{jump if not a zero{23735
{{dcv{7,xl{{{decrement counter{23736
{{brn{6,gts24{{{loop back for next digit{23737
{{ejc{{{{{23738
*      gtstg (continued)
*      loop to generate digits after decimal point
{gts25{cvd{{{{convert a digit into wa{23744
*      merge here first time
{gts26{sch{8,wa{11,-(xr){{store digit{23748
{{dcv{8,wb{{{decrement counter{23749
{{dcv{7,xl{{{decrement counter{23750
{{bnz{7,xl{6,gts25{{loop back if more to go{23751
*      here generate the decimal point
{gts27{mov{8,wa{18,=ch_dt{{load decimal point{23755
{{sch{8,wa{11,-(xr){{store in work area{23756
{{dcv{8,wb{{{decrement counter{23757
*      here generate the digits before the decimal point
{gts28{cvd{{{{convert a digit into wa{23761
{{sch{8,wa{11,-(xr){{store in work area{23762
{{dcv{8,wb{{{decrement counter{23763
{{ine{6,gts28{{{loop back if more to go{23764
{{csc{7,xr{{{complete store characters{23765
{{brn{6,gts08{{{else jump back to exit{23766
*      exit point after successful conversion
{gts29{mov{7,xl{10,(xs)+{{restore xl{23772
{{ica{7,xs{{{pop argument{23773
{{mov{8,wb{3,gtsvb{{restore wb{23774
{{mov{8,wc{3,gtsvc{{restore wc{23775
*      merge here if no conversion required
{gts30{mov{8,wa{13,sclen(xr){{load string length{23779
{{exi{{{{return to caller{23780
*      here to return string for real zero
{gts31{mov{7,xl{21,=scre0{{point to string{23786
{{mov{8,wa{18,=num02{{2 chars{23787
{{zer{8,wb{{{zero offset{23788
{{jsr{6,sbstr{{{copy string{23789
{{brn{6,gts29{{{return{23790
{{enp{{{{end procedure gtstg{23817
{{ejc{{{{{23818
*      gtvar -- get variable for i/o/trace association
*      gtvar is used to point to an actual variable location
*      for the detach,input,output,trace,stoptr system functions
*      (xr)                  argument to function
*      jsr  gtvar            call to locate variable pointer
*      ppm  loc              transfer loc if not ok variable
*      (xl,wa)               name base,offset of variable
*      (xr,ra)               destroyed
*      (wb,wc)               destroyed (convert error only)
*      (xr)                  input arg (convert error only)
{gtvar{prc{25,e{1,1{{entry point{23833
{{bne{9,(xr){22,=b_nml{6,gtvr2{jump if not a name{23834
{{mov{8,wa{13,nmofs(xr){{else load name offset{23835
{{mov{7,xl{13,nmbas(xr){{load name base{23836
{{beq{9,(xl){22,=b_evt{6,gtvr1{error if expression variable{23837
{{bne{9,(xl){22,=b_kvt{6,gtvr3{all ok if not keyword variable{23838
*      here on conversion error
{gtvr1{exi{1,1{{{take convert error exit{23842
*      here if not a name, try convert to natural variable
{gtvr2{mov{3,gtvrc{8,wc{{save wc{23846
{{jsr{6,gtnvr{{{locate vrblk if possible{23847
{{ppm{6,gtvr1{{{jump if convert error{23848
{{mov{7,xl{7,xr{{else copy vrblk name base{23849
{{mov{8,wa{19,*vrval{{and set offset{23850
{{mov{8,wc{3,gtvrc{{restore wc{23851
*      here for name obtained
{gtvr3{bhi{7,xl{3,state{6,gtvr4{all ok if not natural variable{23855
{{beq{13,vrsto(xl){22,=b_vre{6,gtvr1{error if protected variable{23856
*      common exit point
{gtvr4{exi{{{{return to caller{23860
{{enp{{{{end procedure gtvar{23861
{{ejc{{{{{23862
{{ejc{{{{{23863
*      hashs -- compute hash index for string
*      hashs is used to convert a string to a unique integer
*      value. the resulting hash value is a positive integer
*      in the range 0 to cfp_m
*      (xr)                  string to be hashed
*      jsr  hashs            call to hash string
*      (ia)                  hash value
*      (xr,wb,wc)            destroyed
*      the hash function used is as follows.
*      start with the length of the string.
*      if there is more than one character in a word,
*      take the first e_hnw words of the characters from
*      the string or all the words if fewer than e_hnw.
*      compute the exclusive or of all these words treating
*      them as one word bit string values.
*      if there is just one character in a word,
*      then mimic the word by word hash by shifting
*      successive characters to get a similar effect.
*      e_hnw is set to zero in case only one character per word.
*      move the result as an integer with the mti instruction.
*      the test on e_hnw is done dynamically. this should be done
*      eventually using conditional assembly, but that would require
*      changes to the build process (ds 8 may 2013).
{hashs{prc{25,e{1,0{{entry point{23899
*z-
{{mov{8,wc{18,=e_hnw{{get number of words to use{23901
{{bze{8,wc{6,hshsa{{branch if one character per word{23902
{{mov{8,wc{13,sclen(xr){{load string length in characters{23903
{{mov{8,wb{8,wc{{initialize with length{23904
{{bze{8,wc{6,hshs3{{jump if null string{23905
{{zgb{8,wb{{{correct byte ordering if necessary{23906
{{ctw{8,wc{1,0{{get number of words of chars{23907
{{add{7,xr{19,*schar{{point to characters of string{23908
{{blo{8,wc{18,=e_hnw{6,hshs1{use whole string if short{23909
{{mov{8,wc{18,=e_hnw{{else set to involve first e_hnw wds{23910
*      here with count of words to check in wc
{hshs1{lct{8,wc{8,wc{{set counter to control loop{23914
*      loop to compute exclusive or
{hshs2{xob{8,wb{10,(xr)+{{exclusive or next word of chars{23918
{{bct{8,wc{6,hshs2{{loop till all processed{23919
*      merge here with exclusive or in wb
{hshs3{zgb{8,wb{{{zeroise undefined bits{23923
{{anb{8,wb{4,bitsm{{ensure in range 0 to cfp_m{23924
{{mti{8,wb{{{move result as integer{23925
{{zer{7,xr{{{clear garbage value in xr{23926
{{exi{{{{return to hashs caller{23927
*      here if just one character per word
{hshsa{mov{8,wc{13,sclen(xr){{load string length in characters{23931
{{mov{8,wb{8,wc{{initialize with length{23932
{{bze{8,wc{6,hshs3{{jump if null string{23933
{{zgb{8,wb{{{correct byte ordering if necessary{23934
{{ctw{8,wc{1,0{{get number of words of chars{23935
{{plc{7,xr{{{{23936
{{mov{11,-(xs){7,xl{{save xl{23937
{{mov{7,xl{8,wc{{load length for branch{23938
{{bge{7,xl{18,=num25{6,hsh24{use first characters if longer{23939
{{bsw{7,xl{1,25{{merge to compute hash{23940
{{iff{1,0{6,hsh00{{{23966
{{iff{1,1{6,hsh01{{{23966
{{iff{1,2{6,hsh02{{{23966
{{iff{1,3{6,hsh03{{{23966
{{iff{1,4{6,hsh04{{{23966
{{iff{1,5{6,hsh05{{{23966
{{iff{1,6{6,hsh06{{{23966
{{iff{1,7{6,hsh07{{{23966
{{iff{1,8{6,hsh08{{{23966
{{iff{1,9{6,hsh09{{{23966
{{iff{1,10{6,hsh10{{{23966
{{iff{1,11{6,hsh11{{{23966
{{iff{1,12{6,hsh12{{{23966
{{iff{1,13{6,hsh13{{{23966
{{iff{1,14{6,hsh14{{{23966
{{iff{1,15{6,hsh15{{{23966
{{iff{1,16{6,hsh16{{{23966
{{iff{1,17{6,hsh17{{{23966
{{iff{1,18{6,hsh18{{{23966
{{iff{1,19{6,hsh19{{{23966
{{iff{1,20{6,hsh20{{{23966
{{iff{1,21{6,hsh21{{{23966
{{iff{1,22{6,hsh22{{{23966
{{iff{1,23{6,hsh23{{{23966
{{iff{1,24{6,hsh24{{{23966
{{esw{{{{{23966
{hsh24{lch{8,wc{10,(xr)+{{load next character{23967
{{lsh{8,wc{1,24{{shift for hash{23968
{{xob{8,wb{8,wc{{hash character{23969
{hsh23{lch{8,wc{10,(xr)+{{load next character{23970
{{lsh{8,wc{1,16{{shift for hash{23971
{{xob{8,wb{8,wc{{hash character{23972
{hsh22{lch{8,wc{10,(xr)+{{load next character{23973
{{lsh{8,wc{1,8{{shift for hash{23974
{{xob{8,wb{8,wc{{hash character{23975
{hsh21{lch{8,wc{10,(xr)+{{load next character{23976
{{xob{8,wb{8,wc{{hash character{23977
{hsh20{lch{8,wc{10,(xr)+{{load next character{23978
{{lsh{8,wc{1,24{{shift for hash{23979
{{xob{8,wb{8,wc{{hash character{23980
{hsh19{lch{8,wc{10,(xr)+{{load next character{23981
{{lsh{8,wc{1,16{{shift for hash{23982
{{xob{8,wb{8,wc{{hash character{23983
{hsh18{lch{8,wc{10,(xr)+{{load next character{23984
{{lsh{8,wc{1,8{{shift for hash{23985
{{xob{8,wb{8,wc{{hash character{23986
{hsh17{lch{8,wc{10,(xr)+{{load next character{23987
{{xob{8,wb{8,wc{{hash character{23988
{hsh16{lch{8,wc{10,(xr)+{{load next character{23989
{{lsh{8,wc{1,24{{shift for hash{23990
{{xob{8,wb{8,wc{{hash character{23991
{hsh15{lch{8,wc{10,(xr)+{{load next character{23992
{{lsh{8,wc{1,16{{shift for hash{23993
{{xob{8,wb{8,wc{{hash character{23994
{hsh14{lch{8,wc{10,(xr)+{{load next character{23995
{{lsh{8,wc{1,8{{shift for hash{23996
{{xob{8,wb{8,wc{{hash character{23997
{hsh13{lch{8,wc{10,(xr)+{{load next character{23998
{{xob{8,wb{8,wc{{hash character{23999
{hsh12{lch{8,wc{10,(xr)+{{load next character{24000
{{lsh{8,wc{1,24{{shift for hash{24001
{{xob{8,wb{8,wc{{hash character{24002
{hsh11{lch{8,wc{10,(xr)+{{load next character{24003
{{lsh{8,wc{1,16{{shift for hash{24004
{{xob{8,wb{8,wc{{hash character{24005
{hsh10{lch{8,wc{10,(xr)+{{load next character{24006
{{lsh{8,wc{1,8{{shift for hash{24007
{{xob{8,wb{8,wc{{hash character{24008
{hsh09{lch{8,wc{10,(xr)+{{load next character{24009
{{xob{8,wb{8,wc{{hash character{24010
{hsh08{lch{8,wc{10,(xr)+{{load next character{24011
{{lsh{8,wc{1,24{{shift for hash{24012
{{xob{8,wb{8,wc{{hash character{24013
{hsh07{lch{8,wc{10,(xr)+{{load next character{24014
{{lsh{8,wc{1,16{{shift for hash{24015
{{xob{8,wb{8,wc{{hash character{24016
{hsh06{lch{8,wc{10,(xr)+{{load next character{24017
{{lsh{8,wc{1,8{{shift for hash{24018
{{xob{8,wb{8,wc{{hash character{24019
{hsh05{lch{8,wc{10,(xr)+{{load next character{24020
{{xob{8,wb{8,wc{{hash character{24021
{hsh04{lch{8,wc{10,(xr)+{{load next character{24022
{{lsh{8,wc{1,24{{shift for hash{24023
{{xob{8,wb{8,wc{{hash character{24024
{hsh03{lch{8,wc{10,(xr)+{{load next character{24025
{{lsh{8,wc{1,16{{shift for hash{24026
{{xob{8,wb{8,wc{{hash character{24027
{hsh02{lch{8,wc{10,(xr)+{{load next character{24028
{{lsh{8,wc{1,8{{shift for hash{24029
{{xob{8,wb{8,wc{{hash character{24030
{hsh01{lch{8,wc{10,(xr)+{{load next character{24031
{{xob{8,wb{8,wc{{hash character{24032
{hsh00{mov{7,xl{10,(xs)+{{restore xl{24033
{{brn{6,hshs3{{{merge to complete hash{24034
{{enp{{{{end procedure hashs{24035
*      icbld -- build integer block
*      (ia)                  integer value for icblk
*      jsr  icbld            call to build integer block
*      (xr)                  pointer to result icblk
*      (wa)                  destroyed
{icbld{prc{25,e{1,0{{entry point{24044
*z+
{{mfi{7,xr{6,icbl1{{copy small integers{24046
{{ble{7,xr{18,=num02{6,icbl3{jump if 0,1 or 2{24047
*      construct icblk
{icbl1{mov{7,xr{3,dnamp{{load pointer to next available loc{24051
{{add{7,xr{19,*icsi_{{point past new icblk{24052
{{blo{7,xr{3,dname{6,icbl2{jump if there is room{24053
{{mov{8,wa{19,*icsi_{{else load length of icblk{24054
{{jsr{6,alloc{{{use standard allocator to get block{24055
{{add{7,xr{8,wa{{point past block to merge{24056
*      merge here with xr pointing past the block obtained
{icbl2{mov{3,dnamp{7,xr{{set new pointer{24060
{{sub{7,xr{19,*icsi_{{point back to start of block{24061
{{mov{9,(xr){22,=b_icl{{store type word{24062
{{sti{13,icval(xr){{{store integer value in icblk{24063
{{exi{{{{return to icbld caller{24064
*      optimise by not building icblks for small integers
{icbl3{wtb{7,xr{{{convert integer to offset{24068
{{mov{7,xr{14,intab(xr){{point to pre-built icblk{24069
{{exi{{{{return{24070
{{enp{{{{end procedure icbld{24071
{{ejc{{{{{24072
*      ident -- compare two values
*      ident compares two values in the sense of the ident
*      differ functions available at the snobol level.
*      (xr)                  first argument
*      (xl)                  second argument
*      jsr  ident            call to compare arguments
*      ppm  loc              transfer loc if ident
*      (normal return if differ)
*      (xr,xl,wc,ra)         destroyed
{ident{prc{25,e{1,1{{entry point{24086
{{beq{7,xr{7,xl{6,iden7{jump if same pointer (ident){24087
{{mov{8,wc{9,(xr){{else load arg 1 type word{24088
{{bne{8,wc{9,(xl){6,iden1{differ if arg 2 type word differ{24090
{{beq{8,wc{22,=b_scl{6,iden2{jump if strings{24094
{{beq{8,wc{22,=b_icl{6,iden4{jump if integers{24095
{{beq{8,wc{22,=b_rcl{6,iden5{jump if reals{24098
{{beq{8,wc{22,=b_nml{6,iden6{jump if names{24100
*      for all other datatypes, must be differ if xr ne xl
*      merge here for differ
{iden1{exi{{{{take differ exit{24143
*      here for strings, ident only if lengths and chars same
{iden2{mov{8,wc{13,sclen(xr){{load arg 1 length{24147
{{bne{8,wc{13,sclen(xl){6,iden1{differ if lengths differ{24148
*      buffer and string comparisons merge here
{idn2a{add{7,xr{19,*schar{{point to chars of arg 1{24152
{{add{7,xl{19,*schar{{point to chars of arg 2{24153
{{ctw{8,wc{1,0{{get number of words in strings{24154
{{lct{8,wc{8,wc{{set loop counter{24155
*      loop to compare characters. note that wc cannot be zero
*      since all null strings point to nulls and give xl=xr.
{iden3{cne{9,(xr){9,(xl){6,iden8{differ if chars do not match{24160
{{ica{7,xr{{{else bump arg one pointer{24161
{{ica{7,xl{{{bump arg two pointer{24162
{{bct{8,wc{6,iden3{{loop back till all checked{24163
{{ejc{{{{{24164
*      ident (continued)
*      here to exit for case of two ident strings
{{zer{7,xl{{{clear garbage value in xl{24170
{{zer{7,xr{{{clear garbage value in xr{24171
{{exi{1,1{{{take ident exit{24172
*      here for integers, ident if same values
{iden4{ldi{13,icval(xr){{{load arg 1{24176
{{sbi{13,icval(xl){{{subtract arg 2 to compare{24177
{{iov{6,iden1{{{differ if overflow{24178
{{ine{6,iden1{{{differ if result is not zero{24179
{{exi{1,1{{{take ident exit{24180
*      here for reals, ident if same values
{iden5{ldr{13,rcval(xr){{{load arg 1{24186
{{sbr{13,rcval(xl){{{subtract arg 2 to compare{24187
{{rov{6,iden1{{{differ if overflow{24188
{{rne{6,iden1{{{differ if result is not zero{24189
{{exi{1,1{{{take ident exit{24190
*      here for names, ident if bases and offsets same
{iden6{bne{13,nmofs(xr){13,nmofs(xl){6,iden1{differ if different offset{24195
{{bne{13,nmbas(xr){13,nmbas(xl){6,iden1{differ if different base{24196
*      merge here to signal ident for identical pointers
{iden7{exi{1,1{{{take ident exit{24200
*      here for differ strings
{iden8{zer{7,xr{{{clear garbage ptr in xr{24204
{{zer{7,xl{{{clear garbage ptr in xl{24205
{{exi{{{{return to caller (differ){24206
{{enp{{{{end procedure ident{24207
{{ejc{{{{{24208
*      inout - used to initialise input and output variables
*      (xl)                  pointer to vbl name string
*      (wb)                  trblk type
*      jsr  inout            call to perform initialisation
*      (xl)                  vrblk ptr
*      (xr)                  trblk ptr
*      (wa,wc)               destroyed
*      note that trter (= trtrf) field of standard i/o variables
*      points to corresponding svblk not to a trblk as is the
*      case for ordinary variables.
{inout{prc{25,e{1,0{{entry point{24223
{{mov{11,-(xs){8,wb{{stack trblk type{24224
{{mov{8,wa{13,sclen(xl){{get name length{24225
{{zer{8,wb{{{point to start of name{24226
{{jsr{6,sbstr{{{build a proper scblk{24227
{{jsr{6,gtnvr{{{build vrblk{24228
{{ppm{{{{no error return{24229
{{mov{8,wc{7,xr{{save vrblk pointer{24230
{{mov{8,wb{10,(xs)+{{get trter field{24231
{{zer{7,xl{{{zero trfpt{24232
{{jsr{6,trbld{{{build trblk{24233
{{mov{7,xl{8,wc{{recall vrblk pointer{24234
{{mov{13,trter(xr){13,vrsvp(xl){{store svblk pointer{24235
{{mov{13,vrval(xl){7,xr{{store trblk ptr in vrblk{24236
{{mov{13,vrget(xl){22,=b_vra{{set trapped access{24237
{{mov{13,vrsto(xl){22,=b_vrv{{set trapped store{24238
{{exi{{{{return to caller{24239
{{enp{{{{end procedure inout{24240
{{ejc{{{{{24241
*      insta - used to initialize structures in static region
*      (xr)                  pointer to starting static location
*      jsr  insta            call to initialize static structure
*      (xr)                  ptr to next free static location
*      (wa,wb,wc)            destroyed
*      note that this procedure establishes the pointers
*      prbuf, gtswk, and kvalp.
{insta{prc{25,e{1,0{{entry point{24420
*      initialize print buffer with blank words
*z-
{{mov{8,wc{3,prlen{{no. of chars in print bfr{24425
{{mov{3,prbuf{7,xr{{print bfr is put at static start{24426
{{mov{10,(xr)+{22,=b_scl{{store string type code{24427
{{mov{10,(xr)+{8,wc{{and string length{24428
{{ctw{8,wc{1,0{{get number of words in buffer{24429
{{mov{3,prlnw{8,wc{{store for buffer clear{24430
{{lct{8,wc{8,wc{{words to clear{24431
*      loop to clear buffer
{inst1{mov{10,(xr)+{4,nullw{{store blank{24435
{{bct{8,wc{6,inst1{{loop{24436
*      allocate work area for gtstg conversion procedure
{{mov{8,wa{18,=nstmx{{get max num chars in output number{24440
{{ctb{8,wa{2,scsi_{{no of bytes needed{24441
{{mov{3,gtswk{7,xr{{store bfr adrs{24442
{{add{7,xr{8,wa{{bump for work bfr{24443
*      build alphabet string for alphabet keyword and replace
{{mov{3,kvalp{7,xr{{save alphabet pointer{24447
{{mov{9,(xr){22,=b_scl{{string blk type{24448
{{mov{8,wc{18,=cfp_a{{no of chars in alphabet{24449
{{mov{13,sclen(xr){8,wc{{store as string length{24450
{{mov{8,wb{8,wc{{copy char count{24451
{{ctb{8,wb{2,scsi_{{no. of bytes needed{24452
{{add{8,wb{7,xr{{current end address for static{24453
{{mov{8,wa{8,wb{{save adrs past alphabet string{24454
{{lct{8,wc{8,wc{{loop counter{24455
{{psc{7,xr{{{point to chars of string{24456
{{zer{8,wb{{{set initial character value{24457
*      loop to enter character codes in order
{inst2{sch{8,wb{10,(xr)+{{store next code{24461
{{icv{8,wb{{{bump code value{24462
{{bct{8,wc{6,inst2{{loop till all stored{24463
{{csc{7,xr{{{complete store characters{24464
{{mov{7,xr{8,wa{{return current static ptr{24465
{{exi{{{{return to caller{24466
{{enp{{{{end procedure insta{24467
{{ejc{{{{{24468
*      iofcb -- get input/output fcblk pointer
*      used by endfile, eject and rewind to find the fcblk
*      (if any) corresponding to their argument.
*      -(xs)                 argument
*      jsr  iofcb            call to find fcblk
*      ppm  loc              arg is an unsuitable name
*      ppm  loc              arg is null string
*      ppm  loc              arg file not found
*      (xs)                  popped
*      (xl)                  ptr to filearg1 vrblk
*      (xr)                  argument
*      (wa)                  fcblk ptr or 0
*      (wb,wc)               destroyed
{iofcb{prc{25,n{1,3{{entry point{24486
*z+
{{jsr{6,gtstg{{{get arg as string{24488
{{ppm{6,iofc2{{{fail{24489
{{mov{7,xl{7,xr{{copy string ptr{24490
{{jsr{6,gtnvr{{{get as natural variable{24491
{{ppm{6,iofc3{{{fail if null{24492
{{mov{8,wb{7,xl{{copy string pointer again{24493
{{mov{7,xl{7,xr{{copy vrblk ptr for return{24494
{{zer{8,wa{{{in case no trblk found{24495
*      loop to find file arg1 trblk
{iofc1{mov{7,xr{13,vrval(xr){{get possible trblk ptr{24499
{{bne{9,(xr){22,=b_trt{6,iofc4{fail if end of chain{24500
{{bne{13,trtyp(xr){18,=trtfc{6,iofc1{loop if not file arg trblk{24501
{{mov{8,wa{13,trfpt(xr){{get fcblk ptr{24502
{{mov{7,xr{8,wb{{copy arg{24503
{{exi{{{{return{24504
*      fail return
{iofc2{exi{1,1{{{fail{24508
*      null arg
{iofc3{exi{1,2{{{null arg return{24512
*      file not found
{iofc4{exi{1,3{{{file not found return{24516
{{enp{{{{end procedure iofcb{24517
{{ejc{{{{{24518
*      ioppf -- process filearg2 for ioput
*      (r_xsc)               filearg2 ptr
*      jsr  ioppf            call to process filearg2
*      (xl)                  filearg1 ptr
*      (xr)                  file arg2 ptr
*      -(xs)...-(xs)         fields extracted from filearg2
*      (wc)                  no. of fields extracted
*      (wb)                  input/output flag
*      (wa)                  fcblk ptr or 0
{ioppf{prc{25,n{1,0{{entry point{24531
{{zer{8,wb{{{to count fields extracted{24532
*      loop to extract fields
{iopp1{mov{7,xl{18,=iodel{{get delimiter{24536
{{mov{8,wc{7,xl{{copy it{24537
{{zer{8,wa{{{retain leading blanks in filearg2{24538
{{jsr{6,xscan{{{get next field{24539
{{mov{11,-(xs){7,xr{{stack it{24540
{{icv{8,wb{{{increment count{24541
{{bnz{8,wa{6,iopp1{{loop{24542
{{mov{8,wc{8,wb{{count of fields{24543
{{mov{8,wb{3,ioptt{{i/o marker{24544
{{mov{8,wa{3,r_iof{{fcblk ptr or 0{24545
{{mov{7,xr{3,r_io2{{file arg2 ptr{24546
{{mov{7,xl{3,r_io1{{filearg1{24547
{{exi{{{{return{24548
{{enp{{{{end procedure ioppf{24549
{{ejc{{{{{24550
*      ioput -- routine used by input and output
*      ioput sets up input/output  associations. it builds
*      such trace and file control blocks as are necessary and
*      calls sysfc,sysio to perform checks on the
*      arguments and to open the files.
*         +-----------+   +---------------+       +-----------+
*      +-.i           i   i               i------.i   =b_xrt  i
*      i  +-----------+   +---------------+       +-----------+
*      i  /           /        (r_fcb)            i    *4     i
*      i  /           /                           +-----------+
*      i  +-----------+   +---------------+       i           i-
*      i  i   name    +--.i    =b_trt     i       +-----------+
*      i  /           /   +---------------+       i           i
*      i   (first arg)    i =trtin/=trtou i       +-----------+
*      i                  +---------------+             i
*      i                  i     value     i             i
*      i                  +---------------+             i
*      i                  i(trtrf) 0   or i--+          i
*      i                  +---------------+  i          i
*      i                  i(trfpt) 0   or i----+        i
*      i                  +---------------+  i i        i
*      i                     (i/o trblk)     i i        i
*      i  +-----------+                      i i        i
*      i  i           i                      i i        i
*      i  +-----------+                      i i        i
*      i  i           i                      i i        i
*      i  +-----------+   +---------------+  i i        i
*      i  i           +--.i    =b_trt     i.-+ i        i
*      i  +-----------+   +---------------+    i        i
*      i  /           /   i    =trtfc     i    i        i
*      i  /           /   +---------------+    i        i
*      i    (filearg1     i     value     i    i        i
*      i         vrblk)   +---------------+    i        i
*      i                  i(trtrf) 0   or i--+ i        .
*      i                  +---------------+  i .  +-----------+
*      i                  i(trfpt) 0   or i------./   fcblk   /
*      i                  +---------------+  i    +-----------+
*      i                       (trtrf)       i
*      i                                     i
*      i                                     i
*      i                  +---------------+  i
*      i                  i    =b_xrt     i.-+
*      i                  +---------------+
*      i                  i      *5       i
*      i                  +---------------+
*      +------------------i               i
*                         +---------------+       +-----------+
*                         i(trtrf) o   or i------.i  =b_xrt   i
*                         +---------------+       +-----------+
*                         i  name offset  i       i    etc    i
*                         +---------------+
*                           (iochn - chain of name pointers)
{{ejc{{{{{24606
*      ioput (continued)
*      no additional trap blocks are used for standard input/out
*      files. otherwise an i/o trap block is attached to second
*      arg (filearg1) vrblk. see diagram above for details of
*      the structure built.
*      -(xs)                 1st arg (vbl to be associated)
*      -(xs)                 2nd arg (file arg1)
*      -(xs)                 3rd arg (file arg2)
*      (wb)                  0 for input, 3 for output assoc.
*      jsr  ioput            call for input/output association
*      ppm  loc              3rd arg not a string
*      ppm  loc              2nd arg not a suitable name
*      ppm  loc              1st arg not a suitable name
*      ppm  loc              inappropriate file spec for i/o
*      ppm  loc              i/o file does not exist
*      ppm  loc              i/o file cannot be read/written
*      ppm  loc              i/o fcblk currently in use
*      (xs)                  popped
*      (xl,xr,wa,wb,wc)      destroyed
{ioput{prc{25,n{1,7{{entry point{24630
{{zer{3,r_iot{{{in case no trtrf block used{24631
{{zer{3,r_iof{{{in case no fcblk alocated{24632
{{zer{3,r_iop{{{in case sysio fails{24633
{{mov{3,ioptt{8,wb{{store i/o trace type{24634
{{jsr{6,xscni{{{prepare to scan filearg2{24635
{{ppm{6,iop13{{{fail{24636
{{ppm{6,iopa0{{{null file arg2{24637
{iopa0{mov{3,r_io2{7,xr{{keep file arg2{24639
{{mov{7,xl{8,wa{{copy length{24640
{{jsr{6,gtstg{{{convert filearg1 to string{24641
{{ppm{6,iop14{{{fail{24642
{{mov{3,r_io1{7,xr{{keep filearg1 ptr{24643
{{jsr{6,gtnvr{{{convert to natural variable{24644
{{ppm{6,iop00{{{jump if null{24645
{{brn{6,iop04{{{jump to process non-null args{24646
*      null filearg1
{iop00{bze{7,xl{6,iop01{{skip if both args null{24650
{{jsr{6,ioppf{{{process filearg2{24651
{{jsr{6,sysfc{{{call for filearg2 check{24652
{{ppm{6,iop16{{{fail{24653
{{ppm{6,iop26{{{fail{24654
{{brn{6,iop11{{{complete file association{24655
{{ejc{{{{{24656
*      ioput (continued)
*      here with 0 or fcblk ptr in (xl)
{iop01{mov{8,wb{3,ioptt{{get trace type{24662
{{mov{7,xr{3,r_iot{{get 0 or trtrf ptr{24663
{{jsr{6,trbld{{{build trblk{24664
{{mov{8,wc{7,xr{{copy trblk pointer{24665
{{mov{7,xr{10,(xs)+{{get variable from stack{24666
{{mov{11,-(xs){8,wc{{make trblk collectable{24667
{{jsr{6,gtvar{{{point to variable{24668
{{ppm{6,iop15{{{fail{24669
{{mov{8,wc{10,(xs)+{{recover trblk pointer{24670
{{mov{3,r_ion{7,xl{{save name pointer{24671
{{mov{7,xr{7,xl{{copy name pointer{24672
{{add{7,xr{8,wa{{point to variable{24673
{{sub{7,xr{19,*vrval{{subtract offset,merge into loop{24674
*      loop to end of trblk chain if any
{iop02{mov{7,xl{7,xr{{copy blk ptr{24678
{{mov{7,xr{13,vrval(xr){{load ptr to next trblk{24679
{{bne{9,(xr){22,=b_trt{6,iop03{jump if not trapped{24680
{{bne{13,trtyp(xr){3,ioptt{6,iop02{loop if not same assocn{24681
{{mov{7,xr{13,trnxt(xr){{get value and delete old trblk{24682
*      ioput (continued)
*      store new association
{iop03{mov{13,vrval(xl){8,wc{{link to this trblk{24688
{{mov{7,xl{8,wc{{copy pointer{24689
{{mov{13,trnxt(xl){7,xr{{store value in trblk{24690
{{mov{7,xr{3,r_ion{{restore possible vrblk pointer{24691
{{mov{8,wb{8,wa{{keep offset to name{24692
{{jsr{6,setvr{{{if vrblk, set vrget,vrsto{24693
{{mov{7,xr{3,r_iot{{get 0 or trtrf ptr{24694
{{bnz{7,xr{6,iop19{{jump if trtrf block exists{24695
{{exi{{{{return to caller{24696
*      non standard file
*      see if an fcblk has already been allocated.
{iop04{zer{8,wa{{{in case no fcblk found{24701
{{ejc{{{{{24702
*      ioput (continued)
*      search possible trblk chain to pick up the fcblk
{iop05{mov{8,wb{7,xr{{remember blk ptr{24708
{{mov{7,xr{13,vrval(xr){{chain along{24709
{{bne{9,(xr){22,=b_trt{6,iop06{jump if end of trblk chain{24710
{{bne{13,trtyp(xr){18,=trtfc{6,iop05{loop if more to go{24711
{{mov{3,r_iot{7,xr{{point to file arg1 trblk{24712
{{mov{8,wa{13,trfpt(xr){{get fcblk ptr from trblk{24713
*      wa = 0 or fcblk ptr
*      wb = ptr to preceding blk to which any trtrf block
*           for file arg1 must be chained.
{iop06{mov{3,r_iof{8,wa{{keep possible fcblk ptr{24719
{{mov{3,r_iop{8,wb{{keep preceding blk ptr{24720
{{jsr{6,ioppf{{{process filearg2{24721
{{jsr{6,sysfc{{{see if fcblk required{24722
{{ppm{6,iop16{{{fail{24723
{{ppm{6,iop26{{{fail{24724
{{bze{8,wa{6,iop12{{skip if no new fcblk wanted{24725
{{blt{8,wc{18,=num02{6,iop6a{jump if fcblk in dynamic{24726
{{jsr{6,alost{{{get it in static{24727
{{brn{6,iop6b{{{skip{24728
*      obtain fcblk in dynamic
{iop6a{jsr{6,alloc{{{get space for fcblk{24732
*      merge
{iop6b{mov{7,xl{7,xr{{point to fcblk{24736
{{mov{8,wb{8,wa{{copy its length{24737
{{btw{8,wb{{{get count as words (sgd apr80){24738
{{lct{8,wb{8,wb{{loop counter{24739
*      clear fcblk
{iop07{zer{10,(xr)+{{{clear a word{24743
{{bct{8,wb{6,iop07{{loop{24744
{{beq{8,wc{18,=num02{6,iop09{skip if in static - dont set fields{24745
{{mov{9,(xl){22,=b_xnt{{store xnblk code in case{24746
{{mov{13,num01(xl){8,wa{{store length{24747
{{bnz{8,wc{6,iop09{{jump if xnblk wanted{24748
{{mov{9,(xl){22,=b_xrt{{xrblk code requested{24749
{{ejc{{{{{24751
*      ioput (continued)
*      complete fcblk initialisation
{iop09{mov{7,xr{3,r_iot{{get possible trblk ptr{24756
{{mov{3,r_iof{7,xl{{store fcblk ptr{24757
{{bnz{7,xr{6,iop10{{jump if trblk already found{24758
*      a new trblk is needed
{{mov{8,wb{18,=trtfc{{trtyp for fcblk trap blk{24762
{{jsr{6,trbld{{{make the block{24763
{{mov{3,r_iot{7,xr{{copy trtrf ptr{24764
{{mov{7,xl{3,r_iop{{point to preceding blk{24765
{{mov{13,vrval(xr){13,vrval(xl){{copy value field to trblk{24766
{{mov{13,vrval(xl){7,xr{{link new trblk into chain{24767
{{mov{7,xr{7,xl{{point to predecessor blk{24768
{{jsr{6,setvr{{{set trace intercepts{24769
{{mov{7,xr{13,vrval(xr){{recover trblk ptr{24770
{{brn{6,iop1a{{{store fcblk ptr{24771
*      here if existing trblk
{iop10{zer{3,r_iop{{{do not release if sysio fails{24775
*      xr is ptr to trblk, xl is fcblk ptr or 0
{iop1a{mov{13,trfpt(xr){3,r_iof{{store fcblk ptr{24779
*      call sysio to complete file accessing
{iop11{mov{8,wa{3,r_iof{{copy fcblk ptr or 0{24783
{{mov{8,wb{3,ioptt{{get input/output flag{24784
{{mov{7,xr{3,r_io2{{get file arg2{24785
{{mov{7,xl{3,r_io1{{get file arg1{24786
{{jsr{6,sysio{{{associate to the file{24787
{{ppm{6,iop17{{{fail{24788
{{ppm{6,iop18{{{fail{24789
{{bnz{3,r_iot{6,iop01{{not std input if non-null trtrf blk{24790
{{bnz{3,ioptt{6,iop01{{jump if output{24791
{{bze{8,wc{6,iop01{{no change to standard read length{24792
{{mov{3,cswin{8,wc{{store new read length for std file{24793
{{brn{6,iop01{{{merge to finish the task{24794
*      sysfc may have returned a pointer to a private fcblk
{iop12{bnz{7,xl{6,iop09{{jump if private fcblk{24798
{{brn{6,iop11{{{finish the association{24799
*      failure returns
{iop13{exi{1,1{{{3rd arg not a string{24803
{iop14{exi{1,2{{{2nd arg unsuitable{24804
{iop15{ica{7,xs{{{discard trblk pointer{24805
{{exi{1,3{{{1st arg unsuitable{24806
{iop16{exi{1,4{{{file spec wrong{24807
{iop26{exi{1,7{{{fcblk in use{24808
*      i/o file does not exist
{iop17{mov{7,xr{3,r_iop{{is there a trblk to release{24812
{{bze{7,xr{6,iopa7{{if not{24813
{{mov{7,xl{13,vrval(xr){{point to trblk{24814
{{mov{13,vrval(xr){13,vrval(xl){{unsplice it{24815
{{jsr{6,setvr{{{adjust trace intercepts{24816
{iopa7{exi{1,5{{{i/o file does not exist{24817
*      i/o file cannot be read/written
{iop18{mov{7,xr{3,r_iop{{is there a trblk to release{24821
{{bze{7,xr{6,iopa7{{if not{24822
{{mov{7,xl{13,vrval(xr){{point to trblk{24823
{{mov{13,vrval(xr){13,vrval(xl){{unsplice it{24824
{{jsr{6,setvr{{{adjust trace intercepts{24825
{iopa8{exi{1,6{{{i/o file cannot be read/written{24826
{{ejc{{{{{24827
*      ioput (continued)
*      add to iochn chain of associated variables unless
*      already present.
{iop19{mov{8,wc{3,r_ion{{wc = name base, wb = name offset{24834
*      search loop
{iop20{mov{7,xr{13,trtrf(xr){{next link of chain{24838
{{bze{7,xr{6,iop21{{not found{24839
{{bne{8,wc{13,ionmb(xr){6,iop20{no match{24840
{{beq{8,wb{13,ionmo(xr){6,iop22{exit if matched{24841
{{brn{6,iop20{{{loop{24842
*      not found
{iop21{mov{8,wa{19,*num05{{space needed{24846
{{jsr{6,alloc{{{get it{24847
{{mov{9,(xr){22,=b_xrt{{store xrblk code{24848
{{mov{13,num01(xr){8,wa{{store length{24849
{{mov{13,ionmb(xr){8,wc{{store name base{24850
{{mov{13,ionmo(xr){8,wb{{store name offset{24851
{{mov{7,xl{3,r_iot{{point to trtrf blk{24852
{{mov{8,wa{13,trtrf(xl){{get ptr field contents{24853
{{mov{13,trtrf(xl){7,xr{{store ptr to new block{24854
{{mov{13,trtrf(xr){8,wa{{complete the linking{24855
*      insert fcblk on fcblk chain for sysej, sysxi
{iop22{bze{3,r_iof{6,iop25{{skip if no fcblk{24859
{{mov{7,xl{3,r_fcb{{ptr to head of existing chain{24860
*      see if fcblk already on chain
{iop23{bze{7,xl{6,iop24{{not on if end of chain{24864
{{beq{13,num03(xl){3,r_iof{6,iop25{dont duplicate if find it{24865
{{mov{7,xl{13,num02(xl){{get next link{24866
{{brn{6,iop23{{{loop{24867
*      not found so add an entry for this fcblk
{iop24{mov{8,wa{19,*num04{{space needed{24871
{{jsr{6,alloc{{{get it{24872
{{mov{9,(xr){22,=b_xrt{{store block code{24873
{{mov{13,num01(xr){8,wa{{store length{24874
{{mov{13,num02(xr){3,r_fcb{{store previous link in this node{24875
{{mov{13,num03(xr){3,r_iof{{store fcblk ptr{24876
{{mov{3,r_fcb{7,xr{{insert node into fcblk chain{24877
*      return
{iop25{exi{{{{return to caller{24881
{{enp{{{{end procedure ioput{24882
{{ejc{{{{{24883
*      ktrex -- execute keyword trace
*      ktrex is used to execute a possible keyword trace. it
*      includes the test on trace and tests for trace active.
*      (xl)                  ptr to trblk (or 0 if untraced)
*      jsr  ktrex            call to execute keyword trace
*      (xl,wa,wb,wc)         destroyed
*      (ra)                  destroyed
{ktrex{prc{25,r{1,0{{entry point (recursive){24895
{{bze{7,xl{6,ktrx3{{immediate exit if keyword untraced{24896
{{bze{3,kvtra{6,ktrx3{{immediate exit if trace = 0{24897
{{dcv{3,kvtra{{{else decrement trace{24898
{{mov{11,-(xs){7,xr{{save xr{24899
{{mov{7,xr{7,xl{{copy trblk pointer{24900
{{mov{7,xl{13,trkvr(xr){{load vrblk pointer (nmbas){24901
{{mov{8,wa{19,*vrval{{set name offset{24902
{{bze{13,trfnc(xr){6,ktrx1{{jump if print trace{24903
{{jsr{6,trxeq{{{else execute full trace{24904
{{brn{6,ktrx2{{{and jump to exit{24905
*      here for print trace
{ktrx1{mov{11,-(xs){7,xl{{stack vrblk ptr for kwnam{24909
{{mov{11,-(xs){8,wa{{stack offset for kwnam{24910
{{jsr{6,prtsn{{{print statement number{24911
{{mov{8,wa{18,=ch_am{{load ampersand{24912
{{jsr{6,prtch{{{print ampersand{24913
{{jsr{6,prtnm{{{print keyword name{24914
{{mov{7,xr{21,=tmbeb{{point to blank-equal-blank{24915
{{jsr{6,prtst{{{print blank-equal-blank{24916
{{jsr{6,kwnam{{{get keyword pseudo-variable name{24917
{{mov{3,dnamp{7,xr{{reset ptr to delete kvblk{24918
{{jsr{6,acess{{{get keyword value{24919
{{ppm{{{{failure is impossible{24920
{{jsr{6,prtvl{{{print keyword value{24921
{{jsr{6,prtnl{{{terminate print line{24922
*      here to exit after completing trace
{ktrx2{mov{7,xr{10,(xs)+{{restore entry xr{24926
*      merge here to exit if no trace required
{ktrx3{exi{{{{return to ktrex caller{24930
{{enp{{{{end procedure ktrex{24931
{{ejc{{{{{24932
*      kwnam -- get pseudo-variable name for keyword
*      1(xs)                 name base for vrblk
*      0(xs)                 offset (should be *vrval)
*      jsr  kwnam            call to get pseudo-variable name
*      (xs)                  popped twice
*      (xl,wa)               resulting pseudo-variable name
*      (xr,wa,wb)            destroyed
{kwnam{prc{25,n{1,0{{entry point{24943
{{ica{7,xs{{{ignore name offset{24944
{{mov{7,xr{10,(xs)+{{load name base{24945
{{bge{7,xr{3,state{6,kwnm1{jump if not natural variable name{24946
{{bnz{13,vrlen(xr){6,kwnm1{{error if not system variable{24947
{{mov{7,xr{13,vrsvp(xr){{else point to svblk{24948
{{mov{8,wa{13,svbit(xr){{load bit mask{24949
{{anb{8,wa{4,btknm{{and with keyword bit{24950
{{zrb{8,wa{6,kwnm1{{error if no keyword association{24951
{{mov{8,wa{13,svlen(xr){{else load name length in characters{24952
{{ctb{8,wa{2,svchs{{compute offset to field we want{24953
{{add{7,xr{8,wa{{point to svknm field{24954
{{mov{8,wb{9,(xr){{load svknm value{24955
{{mov{8,wa{19,*kvsi_{{set size of kvblk{24956
{{jsr{6,alloc{{{allocate kvblk{24957
{{mov{9,(xr){22,=b_kvt{{store type word{24958
{{mov{13,kvnum(xr){8,wb{{store keyword number{24959
{{mov{13,kvvar(xr){21,=trbkv{{set dummy trblk pointer{24960
{{mov{7,xl{7,xr{{copy kvblk pointer{24961
{{mov{8,wa{19,*kvvar{{set proper offset{24962
{{exi{{{{return to kvnam caller{24963
*      here if not keyword name
{kwnm1{erb{1,251{26,keyword operand is not name of defined keyword{{{24967
{{enp{{{{end procedure kwnam{24968
{{ejc{{{{{24969
*      lcomp-- compare two strings lexically
*      1(xs)                 first argument
*      0(xs)                 second argument
*      jsr  lcomp            call to compare aruments
*      ppm  loc              transfer loc for arg1 not string
*      ppm  loc              transfer loc for arg2 not string
*      ppm  loc              transfer loc if arg1 llt arg2
*      ppm  loc              transfer loc if arg1 leq arg2
*      ppm  loc              transfer loc if arg1 lgt arg2
*      (the normal return is never taken)
*      (xs)                  popped twice
*      (xr,xl)               destroyed
*      (wa,wb,wc,ra)         destroyed
{lcomp{prc{25,n{1,5{{entry point{24986
{{jsr{6,gtstg{{{convert second arg to string{24988
{{ppm{6,lcmp6{{{jump if second arg not string{24992
{{mov{7,xl{7,xr{{else save pointer{24993
{{mov{8,wc{8,wa{{and length{24994
{{jsr{6,gtstg{{{convert first argument to string{24996
{{ppm{6,lcmp5{{{jump if not string{25000
{{mov{8,wb{8,wa{{save arg 1 length{25001
{{plc{7,xr{{{point to chars of arg 1{25002
{{plc{7,xl{{{point to chars of arg 2{25003
{{blo{8,wa{8,wc{6,lcmp1{jump if arg 1 length is smaller{25015
{{mov{8,wa{8,wc{{else set arg 2 length as smaller{25016
*      here with smaller length in (wa)
{lcmp1{bze{8,wa{6,lcmp7{{if null string, compare lengths{25020
{{cmc{6,lcmp4{6,lcmp3{{compare strings, jump if unequal{25021
{lcmp7{bne{8,wb{8,wc{6,lcmp2{if equal, jump if lengths unequal{25022
{{exi{1,4{{{else identical strings, leq exit{25023
{{ejc{{{{{25024
*      lcomp (continued)
*      here if initial strings identical, but lengths unequal
{lcmp2{bhi{8,wb{8,wc{6,lcmp4{jump if arg 1 length gt arg 2 leng{25030
*      here if first arg llt second arg
{lcmp3{exi{1,3{{{take llt exit{25035
*      here if first arg lgt second arg
{lcmp4{exi{1,5{{{take lgt exit{25039
*      here if first arg is not a string
{lcmp5{exi{1,1{{{take bad first arg exit{25043
*      here for second arg not a string
{lcmp6{exi{1,2{{{take bad second arg error exit{25047
{{enp{{{{end procedure lcomp{25048
{{ejc{{{{{25049
*      listr -- list source line
*      listr is used to list a source line during the initial
*      compilation. it is called from scane and scanl.
*      jsr  listr            call to list line
*      (xr,xl,wa,wb,wc)      destroyed
*      global locations used by listr
*      cnttl                 flag for -title, -stitl
*      erlst                 if listing on account of an error
*      lstid                 include depth of current image
*      lstlc                 count lines on current page
*      lstnp                 max number of lines/page
*      lstpf                 set non-zero if the current source
*                            line has been listed, else zero.
*      lstpg                 compiler listing page number
*      lstsn                 set if stmnt num to be listed
*      r_cim                 pointer to current input line.
*      r_ttl                 title for source listing
*      r_stl                 ptr to sub-title string
*      entry point
{listr{prc{25,e{1,0{{entry point{25088
{{bnz{3,cnttl{6,list5{{jump if -title or -stitl{25089
{{bnz{3,lstpf{6,list4{{immediate exit if already listed{25090
{{bge{3,lstlc{3,lstnp{6,list6{jump if no room{25091
*      here after printing title (if needed)
{list0{mov{7,xr{3,r_cim{{load pointer to current image{25095
{{bze{7,xr{6,list4{{jump if no image to print{25096
{{plc{7,xr{{{point to characters{25097
{{lch{8,wa{9,(xr){{load first character{25098
{{mov{7,xr{3,lstsn{{load statement number{25099
{{bze{7,xr{6,list2{{jump if no statement number{25100
{{mti{7,xr{{{else get stmnt number as integer{25101
{{bne{3,stage{18,=stgic{6,list1{skip if execute time{25102
{{beq{8,wa{18,=ch_as{6,list2{no stmnt number list if comment{25103
{{beq{8,wa{18,=ch_mn{6,list2{no stmnt no. if control card{25104
*      print statement number
{list1{jsr{6,prtin{{{else print statement number{25108
{{zer{3,lstsn{{{and clear for next time in{25109
*      here to test for printing include depth
{list2{mov{7,xr{3,lstid{{include depth of image{25114
{{bze{7,xr{6,list8{{if not from an include file{25115
{{mov{8,wa{18,=stnpd{{position for start of statement{25116
{{sub{8,wa{18,=num03{{position to place include depth{25117
{{mov{3,profs{8,wa{{set as starting position{25118
{{mti{7,xr{{{include depth as integer{25119
{{jsr{6,prtin{{{print include depth{25120
{{ejc{{{{{25121
*      listr (continued)
*      here after printing statement number and include depth
{list8{mov{3,profs{18,=stnpd{{point past statement number{25127
{{mov{7,xr{3,r_cim{{load pointer to current image{25137
{{jsr{6,prtst{{{print it{25138
{{icv{3,lstlc{{{bump line counter{25139
{{bnz{3,erlst{6,list3{{jump if error copy to int.ch.{25140
{{jsr{6,prtnl{{{terminate line{25141
{{bze{3,cswdb{6,list3{{jump if -single mode{25142
{{jsr{6,prtnl{{{else add a blank line{25143
{{icv{3,lstlc{{{and bump line counter{25144
*      here after printing source image
{list3{mnz{3,lstpf{{{set flag for line printed{25148
*      merge here to exit
{list4{exi{{{{return to listr caller{25152
*      print title after -title or -stitl card
{list5{zer{3,cnttl{{{clear flag{25156
*      eject to new page and list title
{list6{jsr{6,prtps{{{eject{25160
{{bze{3,prich{6,list7{{skip if listing to regular printer{25161
{{beq{3,r_ttl{21,=nulls{6,list0{terminal listing omits null title{25162
*      list title
{list7{jsr{6,listt{{{list title{25166
{{brn{6,list0{{{merge{25167
{{enp{{{{end procedure listr{25168
{{ejc{{{{{25169
*      listt -- list title and subtitle
*      used during compilation to print page heading
*      jsr  listt            call to list title
*      (xr,wa)               destroyed
{listt{prc{25,e{1,0{{entry point{25178
{{mov{7,xr{3,r_ttl{{point to source listing title{25179
{{jsr{6,prtst{{{print title{25180
{{mov{3,profs{3,lstpo{{set offset{25181
{{mov{7,xr{21,=lstms{{set page message{25182
{{jsr{6,prtst{{{print page message{25183
{{icv{3,lstpg{{{bump page number{25184
{{mti{3,lstpg{{{load page number as integer{25185
{{jsr{6,prtin{{{print page number{25186
{{jsr{6,prtnl{{{terminate title line{25187
{{add{3,lstlc{18,=num02{{count title line and blank line{25188
*      print sub-title (if any)
{{mov{7,xr{3,r_stl{{load pointer to sub-title{25192
{{bze{7,xr{6,lstt1{{jump if no sub-title{25193
{{jsr{6,prtst{{{else print sub-title{25194
{{jsr{6,prtnl{{{terminate line{25195
{{icv{3,lstlc{{{bump line count{25196
*      return point
{lstt1{jsr{6,prtnl{{{print a blank line{25200
{{exi{{{{return to caller{25201
{{enp{{{{end procedure listt{25202
{{ejc{{{{{25203
*      newfn -- record new source file name
*      newfn is used after switching to a new include file, or
*      after a -line statement which contains a file name.
*      (xr)                  file name scblk
*      jsr  newfn
*      (wa,wb,wc,xl,xr,ra)   destroyed
*      on return, the table that maps statement numbers to file
*      names has been updated to include this new file name and
*      the current statement number.  the entry is made only if
*      the file name had changed from its previous value.
{newfn{prc{25,e{1,0{{entry point{25220
{{mov{11,-(xs){7,xr{{save new name{25221
{{mov{7,xl{3,r_sfc{{load previous name{25222
{{jsr{6,ident{{{check for equality{25223
{{ppm{6,nwfn1{{{jump if identical{25224
{{mov{7,xr{10,(xs)+{{different, restore name{25225
{{mov{3,r_sfc{7,xr{{record current file name{25226
{{mov{8,wb{3,cmpsn{{get current statement{25227
{{mti{8,wb{{{convert to integer{25228
{{jsr{6,icbld{{{build icblk for stmt number{25229
{{mov{7,xl{3,r_sfn{{file name table{25230
{{mnz{8,wb{{{lookup statement number by name{25231
{{jsr{6,tfind{{{allocate new teblk{25232
{{ppm{{{{always possible to allocate block{25233
{{mov{13,teval(xl){3,r_sfc{{record file name as entry value{25234
{{exi{{{{{25235
*     here if new name and old name identical
{nwfn1{ica{7,xs{{{pop stack{25239
{{exi{{{{{25240
{{ejc{{{{{25241
*      nexts -- acquire next source image
*      nexts is used to acquire the next source image at compile
*      time. it assumes that a prior call to readr has input
*      a line image (see procedure readr). before the current
*      image is finally lost it may be listed here.
*      jsr  nexts            call to acquire next input line
*      (xr,xl,wa,wb,wc)      destroyed
*      global values affected
*      lstid                 include depth of next image
*      r_cni                 on input, next image. on
*                            exit reset to zero
*      r_cim                 on exit, set to point to image
*      rdcln                 current ln set from next line num
*      scnil                 input image length on exit
*      scnse                 reset to zero on exit
*      lstpf                 set on exit if line is listed
{nexts{prc{25,e{1,0{{entry point{25273
{{bze{3,cswls{6,nxts2{{jump if -nolist{25274
{{mov{7,xr{3,r_cim{{point to image{25275
{{bze{7,xr{6,nxts2{{jump if no image{25276
{{plc{7,xr{{{get char ptr{25277
{{lch{8,wa{9,(xr){{get first char{25278
{{bne{8,wa{18,=ch_mn{6,nxts1{jump if not ctrl card{25279
{{bze{3,cswpr{6,nxts2{{jump if -noprint{25280
*      here to call lister
{nxts1{jsr{6,listr{{{list line{25284
*      here after possible listing
{nxts2{mov{7,xr{3,r_cni{{point to next image{25288
{{mov{3,r_cim{7,xr{{set as next image{25289
{{mov{3,rdcln{3,rdnln{{set as current line number{25290
{{mov{3,lstid{3,cnind{{set as current include depth{25292
{{zer{3,r_cni{{{clear next image pointer{25294
{{mov{8,wa{13,sclen(xr){{get input image length{25295
{{mov{8,wb{3,cswin{{get max allowable length{25296
{{blo{8,wa{8,wb{6,nxts3{skip if not too long{25297
{{mov{8,wa{8,wb{{else truncate{25298
*      here with length in (wa)
{nxts3{mov{3,scnil{8,wa{{use as record length{25302
{{zer{3,scnse{{{reset scnse{25303
{{zer{3,lstpf{{{set line not listed yet{25304
{{exi{{{{return to nexts caller{25305
{{enp{{{{end procedure nexts{25306
{{ejc{{{{{25307
*      patin -- pattern construction for len,pos,rpos,tab,rtab
*      these pattern types all generate a similar node type. so
*      the construction code is shared. see functions section
*      for actual entry points for these five functions.
*      (wa)                  pcode for expression arg case
*      (wb)                  pcode for integer arg case
*      jsr  patin            call to build pattern node
*      ppm  loc              transfer loc for not integer or exp
*      ppm  loc              transfer loc for int out of range
*      (xr)                  pointer to constructed node
*      (xl,wa,wb,wc,ia)      destroyed
{patin{prc{25,n{1,2{{entry point{25323
{{mov{7,xl{8,wa{{preserve expression arg pcode{25324
{{jsr{6,gtsmi{{{try to convert arg as small integer{25325
{{ppm{6,ptin2{{{jump if not integer{25326
{{ppm{6,ptin3{{{jump if out of range{25327
*      common successful exit point
{ptin1{jsr{6,pbild{{{build pattern node{25331
{{exi{{{{return to caller{25332
*      here if argument is not an integer
{ptin2{mov{8,wb{7,xl{{copy expr arg case pcode{25336
{{blo{9,(xr){22,=b_e__{6,ptin1{all ok if expression arg{25337
{{exi{1,1{{{else take error exit for wrong type{25338
*      here for error of out of range integer argument
{ptin3{exi{1,2{{{take out-of-range error exit{25342
{{enp{{{{end procedure patin{25343
{{ejc{{{{{25344
*      patst -- pattern construction for any,notany,
*               break,span and breakx pattern functions.
*      these pattern functions build similar types of nodes and
*      the construction code is shared. see functions section
*      for actual entry points for these five pattern functions.
*      0(xs)                 string argument
*      (wb)                  pcode for one char argument
*      (xl)                  pcode for multi-char argument
*      (wc)                  pcode for expression argument
*      jsr  patst            call to build node
*      ppm  loc              if not string or expr (or null)
*      (xs)                  popped past string argument
*      (xr)                  pointer to constructed node
*      (xl)                  destroyed
*      (wa,wb,wc,ra)         destroyed
*      note that there is a special call to patst in the evals
*      procedure with a slightly different form. see evals
*      for details of the form of this call.
{patst{prc{25,n{1,1{{entry point{25368
{{jsr{6,gtstg{{{convert argument as string{25369
{{ppm{6,pats7{{{jump if not string{25370
{{bze{8,wa{6,pats7{{jump if null string (catspaw){25371
{{bne{8,wa{18,=num01{6,pats2{jump if not one char string{25372
*      here for one char string case
{{bze{8,wb{6,pats2{{treat as multi-char if evals call{25376
{{plc{7,xr{{{point to character{25377
{{lch{7,xr{9,(xr){{load character{25378
*      common exit point after successful construction
{pats1{jsr{6,pbild{{{call routine to build node{25382
{{exi{{{{return to patst caller{25383
{{ejc{{{{{25384
*      patst (continued)
*      here for multi-character string case
{pats2{mov{11,-(xs){7,xl{{save multi-char pcode{25390
{{mov{8,wc{3,ctmsk{{load current mask bit{25391
{{beq{7,xr{3,r_cts{6,pats6{jump if same as last string c3.738{25392
{{mov{11,-(xs){7,xr{{save string pointer{25393
{{lsh{8,wc{1,1{{shift to next position{25394
{{nzb{8,wc{6,pats4{{skip if position left in this tbl{25395
*      here we must allocate a new character table
{{mov{8,wa{19,*ctsi_{{set size of ctblk{25399
{{jsr{6,alloc{{{allocate ctblk{25400
{{mov{3,r_ctp{7,xr{{store ptr to new ctblk{25401
{{mov{10,(xr)+{22,=b_ctt{{store type code, bump ptr{25402
{{lct{8,wb{18,=cfp_a{{set number of words to clear{25403
{{mov{8,wc{4,bits0{{load all zero bits{25404
*      loop to clear all bits in table to zeros
{pats3{mov{10,(xr)+{8,wc{{move word of zero bits{25408
{{bct{8,wb{6,pats3{{loop till all cleared{25409
{{mov{8,wc{4,bits1{{set initial bit position{25410
*      merge here with bit position available
{pats4{mov{3,ctmsk{8,wc{{save parm2 (new bit position){25414
{{mov{7,xl{10,(xs)+{{restore pointer to argument string{25415
{{mov{3,r_cts{7,xl{{save for next time   c3.738{25416
{{mov{8,wb{13,sclen(xl){{load string length{25417
{{bze{8,wb{6,pats6{{jump if null string case{25418
{{lct{8,wb{8,wb{{else set loop counter{25419
{{plc{7,xl{{{point to characters in argument{25420
{{ejc{{{{{25421
*      patst (continued)
*      loop to set bits in column of table
{pats5{lch{8,wa{10,(xl)+{{load next character{25427
{{wtb{8,wa{{{convert to byte offset{25428
{{mov{7,xr{3,r_ctp{{point to ctblk{25429
{{add{7,xr{8,wa{{point to ctblk entry{25430
{{mov{8,wa{8,wc{{copy bit mask{25431
{{orb{8,wa{13,ctchs(xr){{or in bits already set{25432
{{mov{13,ctchs(xr){8,wa{{store resulting bit string{25433
{{bct{8,wb{6,pats5{{loop till all bits set{25434
*      complete processing for multi-char string case
{pats6{mov{7,xr{3,r_ctp{{load ctblk ptr as parm1 for pbild{25438
{{zer{7,xl{{{clear garbage ptr in xl{25439
{{mov{8,wb{10,(xs)+{{load pcode for multi-char str case{25440
{{brn{6,pats1{{{back to exit (wc=bitstring=parm2){25441
*      here if argument is not a string
*      note that the call from evals cannot pass an expression
*      since evalp always reevaluates expressions.
{pats7{mov{8,wb{8,wc{{set pcode for expression argument{25448
{{blo{9,(xr){22,=b_e__{6,pats1{jump to exit if expression arg{25449
{{exi{1,1{{{else take wrong type error exit{25450
{{enp{{{{end procedure patst{25451
{{ejc{{{{{25452
*      pbild -- build pattern node
*      (xr)                  parm1 (only if required)
*      (wb)                  pcode for node
*      (wc)                  parm2 (only if required)
*      jsr  pbild            call to build node
*      (xr)                  pointer to constructed node
*      (wa)                  destroyed
{pbild{prc{25,e{1,0{{entry point{25463
{{mov{11,-(xs){7,xr{{stack possible parm1{25464
{{mov{7,xr{8,wb{{copy pcode{25465
{{lei{7,xr{{{load entry point id (bl_px){25466
{{beq{7,xr{18,=bl_p1{6,pbld1{jump if one parameter{25467
{{beq{7,xr{18,=bl_p0{6,pbld3{jump if no parameters{25468
*      here for two parameter case
{{mov{8,wa{19,*pcsi_{{set size of p2blk{25472
{{jsr{6,alloc{{{allocate block{25473
{{mov{13,parm2(xr){8,wc{{store second parameter{25474
{{brn{6,pbld2{{{merge with one parm case{25475
*      here for one parameter case
{pbld1{mov{8,wa{19,*pbsi_{{set size of p1blk{25479
{{jsr{6,alloc{{{allocate node{25480
*      merge here from two parm case
{pbld2{mov{13,parm1(xr){9,(xs){{store first parameter{25484
{{brn{6,pbld4{{{merge with no parameter case{25485
*      here for case of no parameters
{pbld3{mov{8,wa{19,*pasi_{{set size of p0blk{25489
{{jsr{6,alloc{{{allocate node{25490
*      merge here from other cases
{pbld4{mov{9,(xr){8,wb{{store pcode{25494
{{ica{7,xs{{{pop first parameter{25495
{{mov{13,pthen(xr){21,=ndnth{{set nothen successor pointer{25496
{{exi{{{{return to pbild caller{25497
{{enp{{{{end procedure pbild{25498
{{ejc{{{{{25499
*      pconc -- concatenate two patterns
*      (xl)                  ptr to right pattern
*      (xr)                  ptr to left pattern
*      jsr  pconc            call to concatenate patterns
*      (xr)                  ptr to concatenated pattern
*      (xl,wa,wb,wc)         destroyed
*      to concatenate two patterns, all successors in the left
*      pattern which point to the nothen node must be changed to
*      point to the right pattern. however, this modification
*      must be performed on a copy of the left argument rather
*      than the left argument itself, since the left argument
*      may be pointed to by some other variable value.
*      accordingly, it is necessary to copy the left argument.
*      this is not a trivial process since we must avoid copying
*      nodes more than once and the pattern is a graph structure
*      the following algorithm is employed.
*      the stack is used to store a list of nodes which
*      have already been copied. the format of the entries on
*      this list consists of a two word block. the first word
*      is the old address and the second word is the address
*      of the copy. this list is searched by the pcopy
*      routine to avoid making duplicate copies. a trick is
*      used to accomplish the concatenation at the same time.
*      a special entry is made to start with on the stack. this
*      entry records that the nothen node has been copied
*      already and the address of its copy is the right pattern.
*      this automatically performs the correct replacements.
{pconc{prc{25,e{1,0{{entry point{25534
{{zer{11,-(xs){{{make room for one entry at bottom{25535
{{mov{8,wc{7,xs{{store pointer to start of list{25536
{{mov{11,-(xs){21,=ndnth{{stack nothen node as old node{25537
{{mov{11,-(xs){7,xl{{store right arg as copy of nothen{25538
{{mov{7,xt{7,xs{{initialize pointer to stack entries{25539
{{jsr{6,pcopy{{{copy first node of left arg{25540
{{mov{13,num02(xt){8,wa{{store as result under list{25541
{{ejc{{{{{25542
*      pconc (continued)
*      the following loop scans entries in the list and makes
*      sure that their successors have been copied.
{pcnc1{beq{7,xt{7,xs{6,pcnc2{jump if all entries processed{25549
{{mov{7,xr{11,-(xt){{else load next old address{25550
{{mov{7,xr{13,pthen(xr){{load pointer to successor{25551
{{jsr{6,pcopy{{{copy successor node{25552
{{mov{7,xr{11,-(xt){{load pointer to new node (copy){25553
{{mov{13,pthen(xr){8,wa{{store ptr to new successor{25554
*      now check for special case of alternation node where
*      parm1 points to a node and must be copied like pthen.
{{bne{9,(xr){22,=p_alt{6,pcnc1{loop back if not{25559
{{mov{7,xr{13,parm1(xr){{else load pointer to alternative{25560
{{jsr{6,pcopy{{{copy it{25561
{{mov{7,xr{9,(xt){{restore ptr to new node{25562
{{mov{13,parm1(xr){8,wa{{store ptr to copied alternative{25563
{{brn{6,pcnc1{{{loop back for next entry{25564
*      here at end of copy process
{pcnc2{mov{7,xs{8,wc{{restore stack pointer{25568
{{mov{7,xr{10,(xs)+{{load pointer to copy{25569
{{exi{{{{return to pconc caller{25570
{{enp{{{{end procedure pconc{25571
{{ejc{{{{{25572
*      pcopy -- copy a pattern node
*      pcopy is called from the pconc procedure to copy a single
*      pattern node. the copy is only carried out if the node
*      has not been copied already.
*      (xr)                  pointer to node to be copied
*      (xt)                  ptr to current loc in copy list
*      (wc)                  pointer to list of copied nodes
*      jsr  pcopy            call to copy a node
*      (wa)                  pointer to copy
*      (wb,xr)               destroyed
{pcopy{prc{25,n{1,0{{entry point{25587
{{mov{8,wb{7,xt{{save xt{25588
{{mov{7,xt{8,wc{{point to start of list{25589
*      loop to search list of nodes copied already
{pcop1{dca{7,xt{{{point to next entry on list{25593
{{beq{7,xr{9,(xt){6,pcop2{jump if match{25594
{{dca{7,xt{{{else skip over copied address{25595
{{bne{7,xt{7,xs{6,pcop1{loop back if more to test{25596
*      here if not in list, perform copy
{{mov{8,wa{9,(xr){{load first word of block{25600
{{jsr{6,blkln{{{get length of block{25601
{{mov{7,xl{7,xr{{save pointer to old node{25602
{{jsr{6,alloc{{{allocate space for copy{25603
{{mov{11,-(xs){7,xl{{store old address on list{25604
{{mov{11,-(xs){7,xr{{store new address on list{25605
{{chk{{{{check for stack overflow{25606
{{mvw{{{{move words from old block to copy{25607
{{mov{8,wa{9,(xs){{load pointer to copy{25608
{{brn{6,pcop3{{{jump to exit{25609
*      here if we find entry in list
{pcop2{mov{8,wa{11,-(xt){{load address of copy from list{25613
*      common exit point
{pcop3{mov{7,xt{8,wb{{restore xt{25617
{{exi{{{{return to pcopy caller{25618
{{enp{{{{end procedure pcopy{25619
{{ejc{{{{{25620
*      prflr -- print profile
*      prflr is called to print the contents of the profile
*      table in a fairly readable tabular format.
*      jsr  prflr            call to print profile
*      (wa,ia)               destroyed
{prflr{prc{25,e{1,0{{{25631
{{bze{3,pfdmp{6,prfl4{{no printing if no profiling done{25632
{{mov{11,-(xs){7,xr{{preserve entry xr{25633
{{mov{3,pfsvw{8,wb{{and also wb{25634
{{jsr{6,prtpg{{{eject{25635
{{mov{7,xr{21,=pfms1{{load msg /program profile/{25636
{{jsr{6,prtst{{{and print it{25637
{{jsr{6,prtnl{{{followed by newline{25638
{{jsr{6,prtnl{{{and another{25639
{{mov{7,xr{21,=pfms2{{point to first hdr{25640
{{jsr{6,prtst{{{print it{25641
{{jsr{6,prtnl{{{new line{25642
{{mov{7,xr{21,=pfms3{{second hdr{25643
{{jsr{6,prtst{{{print it{25644
{{jsr{6,prtnl{{{new line{25645
{{jsr{6,prtnl{{{and another blank line{25646
{{zer{8,wb{{{initial stmt count{25647
{{mov{7,xr{3,pftbl{{point to table origin{25648
{{add{7,xr{19,*xndta{{bias past xnblk header (sgd07){25649
*      loop here to print successive entries
{prfl1{icv{8,wb{{{bump stmt nr{25653
{{ldi{9,(xr){{{load nr of executions{25654
{{ieq{6,prfl3{{{no printing if zero{25655
{{mov{3,profs{18,=pfpd1{{point where to print{25656
{{jsr{6,prtin{{{and print it{25657
{{zer{3,profs{{{back to start of line{25658
{{mti{8,wb{{{load stmt nr{25659
{{jsr{6,prtin{{{print it there{25660
{{mov{3,profs{18,=pfpd2{{and pad past count{25661
{{ldi{13,cfp_i(xr){{{load total exec time{25662
{{jsr{6,prtin{{{print that too{25663
{{ldi{13,cfp_i(xr){{{reload time{25664
{{mli{4,intth{{{convert to microsec{25665
{{iov{6,prfl2{{{omit next bit if overflow{25666
{{dvi{9,(xr){{{divide by executions{25667
{{mov{3,profs{18,=pfpd3{{pad last print{25668
{{jsr{6,prtin{{{and print mcsec/execn{25669
*      merge after printing time
{prfl2{jsr{6,prtnl{{{thats another line{25673
*      here to go to next entry
{prfl3{add{7,xr{19,*pf_i2{{bump index ptr (sgd07){25677
{{blt{8,wb{3,pfnte{6,prfl1{loop if more stmts{25678
{{mov{7,xr{10,(xs)+{{restore callers xr{25679
{{mov{8,wb{3,pfsvw{{and wb too{25680
*      here to exit
{prfl4{exi{{{{return{25684
{{enp{{{{end of prflr{25685
{{ejc{{{{{25686
*      prflu -- update an entry in the profile table
*      on entry, kvstn contains nr of stmt to profile
*      jsr  prflu            call to update entry
*      (ia)                  destroyed
{prflu{prc{25,e{1,0{{{25695
{{bnz{3,pffnc{6,pflu4{{skip if just entered function{25696
{{mov{11,-(xs){7,xr{{preserve entry xr{25697
{{mov{3,pfsvw{8,wa{{save wa (sgd07){25698
{{bnz{3,pftbl{6,pflu2{{branch if table allocated{25699
*      here if space for profile table not yet allocated.
*      calculate size needed, allocate a static xnblk, and
*      initialize it all to zero.
*      the time taken for this will be attributed to the current
*      statement (assignment to keywd profile), but since the
*      timing for this statement is up the pole anyway, this
*      doesnt really matter...
{{sub{3,pfnte{18,=num01{{adjust for extra count (sgd07){25709
{{mti{4,pfi2a{{{convrt entry size to int{25710
{{sti{3,pfste{{{and store safely for later{25711
{{mti{3,pfnte{{{load table length as integer{25712
{{mli{3,pfste{{{multiply by entry size{25713
{{mfi{8,wa{{{get back address-style{25714
{{add{8,wa{18,=num02{{add on 2 word overhead{25715
{{wtb{8,wa{{{convert the whole lot to bytes{25716
{{jsr{6,alost{{{gimme the space{25717
{{mov{3,pftbl{7,xr{{save block pointer{25718
{{mov{10,(xr)+{22,=b_xnt{{put block type and ...{25719
{{mov{10,(xr)+{8,wa{{... length into header{25720
{{mfi{8,wa{{{get back nr of wds in data area{25721
{{lct{8,wa{8,wa{{load the counter{25722
*      loop here to zero the block data
{pflu1{zer{10,(xr)+{{{blank a word{25726
{{bct{8,wa{6,pflu1{{and alllllll the rest{25727
*      end of allocation. merge back into routine
{pflu2{mti{3,kvstn{{{load nr of stmt just ended{25731
{{sbi{4,intv1{{{make into index offset{25732
{{mli{3,pfste{{{make offset of table entry{25733
{{mfi{8,wa{{{convert to address{25734
{{wtb{8,wa{{{get as baus{25735
{{add{8,wa{19,*num02{{offset includes table header{25736
{{mov{7,xr{3,pftbl{{get table start{25737
{{bge{8,wa{13,num01(xr){6,pflu3{if out of table, skip it{25738
{{add{7,xr{8,wa{{else point to entry{25739
{{ldi{9,(xr){{{get nr of executions so far{25740
{{adi{4,intv1{{{nudge up one{25741
{{sti{9,(xr){{{and put back{25742
{{jsr{6,systm{{{get time now{25743
{{sti{3,pfetm{{{stash ending time{25744
{{sbi{3,pfstm{{{subtract start time{25745
{{adi{13,cfp_i(xr){{{add cumulative time so far{25746
{{sti{13,cfp_i(xr){{{and put back new total{25747
{{ldi{3,pfetm{{{load end time of this stmt ...{25748
{{sti{3,pfstm{{{... which is start time of next{25749
*      merge here to exit
{pflu3{mov{7,xr{10,(xs)+{{restore callers xr{25753
{{mov{8,wa{3,pfsvw{{restore saved reg{25754
{{exi{{{{and return{25755
*      here if profile is suppressed because a program defined
*      function is about to be entered, and so the current stmt
*      has not yet finished
{pflu4{zer{3,pffnc{{{reset the condition flag{25761
{{exi{{{{and immediate return{25762
{{enp{{{{end of procedure prflu{25763
{{ejc{{{{{25764
*      prpar - process print parameters
*      (wc)                  if nonzero associate terminal only
*      jsr  prpar            call to process print parameters
*      (xl,xr,wa,wb,wc)      destroyed
*      since memory allocation is undecided on initial call,
*      terminal cannot be associated. the entry with wc non-zero
*      is provided so a later call can be made to complete this.
{prpar{prc{25,e{1,0{{entry point{25777
{{bnz{8,wc{6,prpa8{{jump to associate terminal{25778
{{jsr{6,syspp{{{get print parameters{25779
{{bnz{8,wb{6,prpa1{{jump if lines/page specified{25780
{{mov{8,wb{3,mxint{{else use a large value{25781
{{rsh{8,wb{1,1{{but not too large{25782
*      store line count/page
{prpa1{mov{3,lstnp{8,wb{{store number of lines/page{25786
{{mov{3,lstlc{8,wb{{pretend page is full initially{25787
{{zer{3,lstpg{{{clear page number{25788
{{mov{8,wb{3,prlen{{get prior length if any{25789
{{bze{8,wb{6,prpa2{{skip if no length{25790
{{bgt{8,wa{8,wb{6,prpa3{skip storing if too big{25791
*      store print buffer length
{prpa2{mov{3,prlen{8,wa{{store value{25795
*      process bits options
{prpa3{mov{8,wb{4,bits3{{bit 3 mask{25799
{{anb{8,wb{8,wc{{get -nolist bit{25800
{{zrb{8,wb{6,prpa4{{skip if clear{25801
{{zer{3,cswls{{{set -nolist{25802
*      check if fail reports goto interactive channel
{prpa4{mov{8,wb{4,bits1{{bit 1 mask{25806
{{anb{8,wb{8,wc{{get bit{25807
{{mov{3,erich{8,wb{{store int. chan. error flag{25808
{{mov{8,wb{4,bits2{{bit 2 mask{25809
{{anb{8,wb{8,wc{{get bit{25810
{{mov{3,prich{8,wb{{flag for std printer on int. chan.{25811
{{mov{8,wb{4,bits4{{bit 4 mask{25812
{{anb{8,wb{8,wc{{get bit{25813
{{mov{3,cpsts{8,wb{{flag for compile stats suppressn.{25814
{{mov{8,wb{4,bits5{{bit 5 mask{25815
{{anb{8,wb{8,wc{{get bit{25816
{{mov{3,exsts{8,wb{{flag for exec stats suppression{25817
{{ejc{{{{{25818
*      prpar (continued)
{{mov{8,wb{4,bits6{{bit 6 mask{25822
{{anb{8,wb{8,wc{{get bit{25823
{{mov{3,precl{8,wb{{extended/compact listing flag{25824
{{sub{8,wa{18,=num08{{point 8 chars from line end{25825
{{zrb{8,wb{6,prpa5{{jump if not extended{25826
{{mov{3,lstpo{8,wa{{store for listing page headings{25827
*       continue option processing
{prpa5{mov{8,wb{4,bits7{{bit 7 mask{25831
{{anb{8,wb{8,wc{{get bit 7{25832
{{mov{3,cswex{8,wb{{set -noexecute if non-zero{25833
{{mov{8,wb{4,bit10{{bit 10 mask{25834
{{anb{8,wb{8,wc{{get bit 10{25835
{{mov{3,headp{8,wb{{pretend printed to omit headers{25836
{{mov{8,wb{4,bits9{{bit 9 mask{25837
{{anb{8,wb{8,wc{{get bit 9{25838
{{mov{3,prsto{8,wb{{keep it as std listing option{25839
{{mov{8,wb{4,bit12{{bit 12 mask{25846
{{anb{8,wb{8,wc{{get bit 12{25847
{{mov{3,cswer{8,wb{{keep it as errors/noerrors option{25848
{{zrb{8,wb{6,prpa6{{skip if clear{25849
{{mov{8,wa{3,prlen{{get print buffer length{25850
{{sub{8,wa{18,=num08{{point 8 chars from line end{25851
{{mov{3,lstpo{8,wa{{store page offset{25852
*      check for -print/-noprint
{prpa6{mov{8,wb{4,bit11{{bit 11 mask{25856
{{anb{8,wb{8,wc{{get bit 11{25857
{{mov{3,cswpr{8,wb{{set -print if non-zero{25858
*      check for terminal
{{anb{8,wc{4,bits8{{see if terminal to be activated{25862
{{bnz{8,wc{6,prpa8{{jump if terminal required{25863
{{bze{3,initr{6,prpa9{{jump if no terminal to detach{25864
{{mov{7,xl{21,=v_ter{{ptr to /terminal/{25865
{{jsr{6,gtnvr{{{get vrblk pointer{25866
{{ppm{{{{cant fail{25867
{{mov{13,vrval(xr){21,=nulls{{clear value of terminal{25868
{{jsr{6,setvr{{{remove association{25869
{{brn{6,prpa9{{{return{25870
*      associate terminal
{prpa8{mnz{3,initr{{{note terminal associated{25874
{{bze{3,dnamb{6,prpa9{{cant if memory not organised{25875
{{mov{7,xl{21,=v_ter{{point to terminal string{25876
{{mov{8,wb{18,=trtou{{output trace type{25877
{{jsr{6,inout{{{attach output trblk to vrblk{25878
{{mov{11,-(xs){7,xr{{stack trblk ptr{25879
{{mov{7,xl{21,=v_ter{{point to terminal string{25880
{{mov{8,wb{18,=trtin{{input trace type{25881
{{jsr{6,inout{{{attach input trace blk{25882
{{mov{13,vrval(xr){10,(xs)+{{add output trblk to chain{25883
*      return point
{prpa9{exi{{{{return{25887
{{enp{{{{end procedure prpar{25888
{{ejc{{{{{25889
*      prtch -- print a character
*      prtch is used to print a single character
*      (wa)                  character to be printed
*      jsr  prtch            call to print character
{prtch{prc{25,e{1,0{{entry point{25898
{{mov{11,-(xs){7,xr{{save xr{25899
{{bne{3,profs{3,prlen{6,prch1{jump if room in buffer{25900
{{jsr{6,prtnl{{{else print this line{25901
*      here after making sure we have room
{prch1{mov{7,xr{3,prbuf{{point to print buffer{25905
{{psc{7,xr{3,profs{{point to next character location{25906
{{sch{8,wa{9,(xr){{store new character{25907
{{csc{7,xr{{{complete store characters{25908
{{icv{3,profs{{{bump pointer{25909
{{mov{7,xr{10,(xs)+{{restore entry xr{25910
{{exi{{{{return to prtch caller{25911
{{enp{{{{end procedure prtch{25912
{{ejc{{{{{25913
*      prtic -- print to interactive channel
*      prtic is called to print the contents of the standard
*      print buffer to the interactive channel. it is only
*      called after prtst has set up the string for printing.
*      it does not clear the buffer.
*      jsr  prtic            call for print
*      (wa,wb)               destroyed
{prtic{prc{25,e{1,0{{entry point{25925
{{mov{11,-(xs){7,xr{{save xr{25926
{{mov{7,xr{3,prbuf{{point to buffer{25927
{{mov{8,wa{3,profs{{no of chars{25928
{{jsr{6,syspi{{{print{25929
{{ppm{6,prtc2{{{fail return{25930
*      return
{prtc1{mov{7,xr{10,(xs)+{{restore xr{25934
{{exi{{{{return{25935
*      error occured
{prtc2{zer{3,erich{{{prevent looping{25939
{{erb{1,252{26,error on printing to interactive channel{{{25940
{{brn{6,prtc1{{{return{25941
{{enp{{{{procedure prtic{25942
{{ejc{{{{{25943
*      prtis -- print to interactive and standard printer
*      prtis puts a line from the print buffer onto the
*      interactive channel (if any) and the standard printer.
*      it always prints to the standard printer but does
*      not duplicate lines if the standard printer is
*      interactive.  it clears down the print buffer.
*      jsr  prtis            call for printing
*      (wa,wb)               destroyed
{prtis{prc{25,e{1,0{{entry point{25956
{{bnz{3,prich{6,prts1{{jump if standard printer is int.ch.{25957
{{bze{3,erich{6,prts1{{skip if not doing int. error reps.{25958
{{jsr{6,prtic{{{print to interactive channel{25959
*      merge and exit
{prts1{jsr{6,prtnl{{{print to standard printer{25963
{{exi{{{{return{25964
{{enp{{{{end procedure prtis{25965
{{ejc{{{{{25966
*      prtin -- print an integer
*      prtin prints the integer value which is in the integer
*      accumulator. blocks built in dynamic storage
*      during this process are immediately deleted.
*      (ia)                  integer value to be printed
*      jsr  prtin            call to print integer
*      (ia,ra)               destroyed
{prtin{prc{25,e{1,0{{entry point{25978
{{mov{11,-(xs){7,xr{{save xr{25979
{{jsr{6,icbld{{{build integer block{25980
{{blo{7,xr{3,dnamb{6,prti1{jump if icblk below dynamic{25981
{{bhi{7,xr{3,dnamp{6,prti1{jump if above dynamic{25982
{{mov{3,dnamp{7,xr{{immediately delete it{25983
*      delete icblk from dynamic store
{prti1{mov{11,-(xs){7,xr{{stack ptr for gtstg{25987
{{jsr{6,gtstg{{{convert to string{25988
{{ppm{{{{convert error is impossible{25989
{{mov{3,dnamp{7,xr{{reset pointer to delete scblk{25990
{{jsr{6,prtst{{{print integer string{25991
{{mov{7,xr{10,(xs)+{{restore entry xr{25992
{{exi{{{{return to prtin caller{25993
{{enp{{{{end procedure prtin{25994
{{ejc{{{{{25995
*      prtmi -- print message and integer
*      prtmi is used to print messages together with an integer
*      value starting in column 15 (used by the routines at
*      the end of compilation).
*      jsr  prtmi            call to print message and integer
{prtmi{prc{25,e{1,0{{entry point{26005
{{jsr{6,prtst{{{print string message{26006
{{mov{3,profs{18,=prtmf{{set column offset{26007
{{jsr{6,prtin{{{print integer{26008
{{jsr{6,prtnl{{{print line{26009
{{exi{{{{return to prtmi caller{26010
{{enp{{{{end procedure prtmi{26011
{{ejc{{{{{26012
*      prtmm -- print memory used and available
*      prtmm is used to provide memory usage information in
*      both the end-of-compile and end-of-run statistics.
*      jsr  prtmm            call to print memory stats
{prtmm{prc{25,e{1,0{{{26021
{{mov{8,wa{3,dnamp{{next available loc{26022
{{sub{8,wa{3,statb{{minus start{26023
{{mti{8,wa{{{convert to integer{26028
{{mov{7,xr{21,=encm1{{point to /memory used (words)/{26029
{{jsr{6,prtmi{{{print message{26030
{{mov{8,wa{3,dname{{end of memory{26031
{{sub{8,wa{3,dnamp{{minus next available loc{26032
{{mti{8,wa{{{convert to integer{26037
{{mov{7,xr{21,=encm2{{point to /memory available (words)/{26038
{{jsr{6,prtmi{{{print line{26039
{{exi{{{{return to prtmm caller{26040
{{enp{{{{end of procedure prtmm{26041
{{ejc{{{{{26042
*      prtmx  -- as prtmi with extra copy to interactive chan.
*      jsr  prtmx            call for printing
*      (wa,wb)               destroyed
{prtmx{prc{25,e{1,0{{entry point{26049
{{jsr{6,prtst{{{print string message{26050
{{mov{3,profs{18,=prtmf{{set column offset{26051
{{jsr{6,prtin{{{print integer{26052
{{jsr{6,prtis{{{print line{26053
{{exi{{{{return{26054
{{enp{{{{end procedure prtmx{26055
{{ejc{{{{{26056
*      prtnl -- print new line (end print line)
*      prtnl prints the contents of the print buffer, resets
*      the buffer to all blanks and resets the print pointer.
*      jsr  prtnl            call to print line
{prtnl{prc{25,r{1,0{{entry point{26065
{{bnz{3,headp{6,prnl0{{were headers printed{26066
{{jsr{6,prtps{{{no - print them{26067
*      call syspr
{prnl0{mov{11,-(xs){7,xr{{save entry xr{26071
{{mov{3,prtsa{8,wa{{save wa{26072
{{mov{3,prtsb{8,wb{{save wb{26073
{{mov{7,xr{3,prbuf{{load pointer to buffer{26074
{{mov{8,wa{3,profs{{load number of chars in buffer{26075
{{jsr{6,syspr{{{call system print routine{26076
{{ppm{6,prnl2{{{jump if failed{26077
{{lct{8,wa{3,prlnw{{load length of buffer in words{26078
{{add{7,xr{19,*schar{{point to chars of buffer{26079
{{mov{8,wb{4,nullw{{get word of blanks{26080
*      loop to blank buffer
{prnl1{mov{10,(xr)+{8,wb{{store word of blanks, bump ptr{26084
{{bct{8,wa{6,prnl1{{loop till all blanked{26085
*      exit point
{{mov{8,wb{3,prtsb{{restore wb{26089
{{mov{8,wa{3,prtsa{{restore wa{26090
{{mov{7,xr{10,(xs)+{{restore entry xr{26091
{{zer{3,profs{{{reset print buffer pointer{26092
{{exi{{{{return to prtnl caller{26093
*      file full or no output file for load module
{prnl2{bnz{3,prtef{6,prnl3{{jump if not first time{26097
{{mnz{3,prtef{{{mark first occurrence{26098
{{erb{1,253{26,print limit exceeded on standard output channel{{{26099
*      stop at once
{prnl3{mov{8,wb{18,=nini8{{ending code{26103
{{mov{8,wa{3,kvstn{{statement number{26104
{{mov{7,xl{3,r_fcb{{get fcblk chain head{26105
{{jsr{6,sysej{{{stop{26106
{{enp{{{{end procedure prtnl{26107
{{ejc{{{{{26108
*      prtnm -- print variable name
*      prtnm is used to print a character representation of the
*      name of a variable (not a value of datatype name)
*      names of pseudo-variables may not be passed to prtnm.
*      (xl)                  name base
*      (wa)                  name offset
*      jsr  prtnm            call to print name
*      (wb,wc,ra)            destroyed
{prtnm{prc{25,r{1,0{{entry point (recursive, see prtvl){26121
{{mov{11,-(xs){8,wa{{save wa (offset is collectable){26122
{{mov{11,-(xs){7,xr{{save entry xr{26123
{{mov{11,-(xs){7,xl{{save name base{26124
{{bhi{7,xl{3,state{6,prn02{jump if not natural variable{26125
*      here for natural variable name, recognized by the fact
*      that the name base points into the static area.
{{mov{7,xr{7,xl{{point to vrblk{26130
{{jsr{6,prtvn{{{print name of variable{26131
*      common exit point
{prn01{mov{7,xl{10,(xs)+{{restore name base{26135
{{mov{7,xr{10,(xs)+{{restore entry value of xr{26136
{{mov{8,wa{10,(xs)+{{restore wa{26137
{{exi{{{{return to prtnm caller{26138
*      here for case of non-natural variable
{prn02{mov{8,wb{8,wa{{copy name offset{26142
{{bne{9,(xl){22,=b_pdt{6,prn03{jump if array or table{26143
*      for program defined datatype, prt fld name, left paren
{{mov{7,xr{13,pddfp(xl){{load pointer to dfblk{26147
{{add{7,xr{8,wa{{add name offset{26148
{{mov{7,xr{13,pdfof(xr){{load vrblk pointer for field{26149
{{jsr{6,prtvn{{{print field name{26150
{{mov{8,wa{18,=ch_pp{{load left paren{26151
{{jsr{6,prtch{{{print character{26152
{{ejc{{{{{26153
*      prtnm (continued)
*      now we print an identifying name for the object if one
*      can be found. the following code searches for a natural
*      variable which contains this object as value. if such a
*      variable is found, its name is printed, else the value
*      of the object (as printed by prtvl) is used instead.
*      first we point to the parent tbblk if this is the case of
*      a table element. to do this, chase down the trnxt chain.
{prn03{bne{9,(xl){22,=b_tet{6,prn04{jump if we got there (or not te){26166
{{mov{7,xl{13,tenxt(xl){{else move out on chain{26167
{{brn{6,prn03{{{and loop back{26168
*      now we are ready for the search. to speed things up in
*      the case of calls from dump where the same name base
*      will occur repeatedly while dumping an array or table,
*      we remember the last vrblk pointer found in prnmv. so
*      first check to see if we have this one again.
{prn04{mov{7,xr{3,prnmv{{point to vrblk we found last time{26176
{{mov{8,wa{3,hshtb{{point to hash table in case not{26177
{{brn{6,prn07{{{jump into search for special check{26178
*      loop through hash slots
{prn05{mov{7,xr{8,wa{{copy slot pointer{26182
{{ica{8,wa{{{bump slot pointer{26183
{{sub{7,xr{19,*vrnxt{{introduce standard vrblk offset{26184
*      loop through vrblks on one hash chain
{prn06{mov{7,xr{13,vrnxt(xr){{point to next vrblk on hash chain{26188
*      merge here first time to check block we found last time
{prn07{mov{8,wc{7,xr{{copy vrblk pointer{26192
{{bze{8,wc{6,prn09{{jump if chain end (or prnmv zero){26193
{{ejc{{{{{26194
*      prtnm (continued)
*      loop to find value (chase down possible trblk chain)
{prn08{mov{7,xr{13,vrval(xr){{load value{26200
{{beq{9,(xr){22,=b_trt{6,prn08{loop if that was a trblk{26201
*      now we have the value, is this the block we want
{{beq{7,xr{7,xl{6,prn10{jump if this matches the name base{26205
{{mov{7,xr{8,wc{{else point back to that vrblk{26206
{{brn{6,prn06{{{and loop back{26207
*      here to move to next hash slot
{prn09{blt{8,wa{3,hshte{6,prn05{loop back if more to go{26211
{{mov{7,xr{7,xl{{else not found, copy value pointer{26212
{{jsr{6,prtvl{{{print value{26213
{{brn{6,prn11{{{and merge ahead{26214
*      here when we find a matching entry
{prn10{mov{7,xr{8,wc{{copy vrblk pointer{26218
{{mov{3,prnmv{7,xr{{save for next time in{26219
{{jsr{6,prtvn{{{print variable name{26220
*      merge here if no entry found
{prn11{mov{8,wc{9,(xl){{load first word of name base{26224
{{bne{8,wc{22,=b_pdt{6,prn13{jump if not program defined{26225
*      for program defined datatype, add right paren and exit
{{mov{8,wa{18,=ch_rp{{load right paren, merge{26229
*      merge here to print final right paren or bracket
{prn12{jsr{6,prtch{{{print final character{26233
{{mov{8,wa{8,wb{{restore name offset{26234
{{brn{6,prn01{{{merge back to exit{26235
{{ejc{{{{{26236
*      prtnm (continued)
*      here for array or table
{prn13{mov{8,wa{18,=ch_bb{{load left bracket{26242
{{jsr{6,prtch{{{and print it{26243
{{mov{7,xl{9,(xs){{restore block pointer{26244
{{mov{8,wc{9,(xl){{load type word again{26245
{{bne{8,wc{22,=b_tet{6,prn15{jump if not table{26246
*      here for table, print subscript value
{{mov{7,xr{13,tesub(xl){{load subscript value{26250
{{mov{7,xl{8,wb{{save name offset{26251
{{jsr{6,prtvl{{{print subscript value{26252
{{mov{8,wb{7,xl{{restore name offset{26253
*      merge here from array case to print right bracket
{prn14{mov{8,wa{18,=ch_rb{{load right bracket{26257
{{brn{6,prn12{{{merge back to print it{26258
*      here for array or vector, to print subscript(s)
{prn15{mov{8,wa{8,wb{{copy name offset{26262
{{btw{8,wa{{{convert to words{26263
{{beq{8,wc{22,=b_art{6,prn16{jump if arblk{26264
*      here for vector
{{sub{8,wa{18,=vcvlb{{adjust for standard fields{26268
{{mti{8,wa{{{move to integer accum{26269
{{jsr{6,prtin{{{print linear subscript{26270
{{brn{6,prn14{{{merge back for right bracket{26271
{{ejc{{{{{26272
*      prtnm (continued)
*      here for array. first calculate absolute subscript
*      offsets by successive divisions by the dimension values.
*      this must be done right to left since the elements are
*      stored row-wise. the subscripts are stacked as integers.
{prn16{mov{8,wc{13,arofs(xl){{load length of bounds info{26281
{{ica{8,wc{{{adjust for arpro field{26282
{{btw{8,wc{{{convert to words{26283
{{sub{8,wa{8,wc{{get linear zero-origin subscript{26284
{{mti{8,wa{{{get integer value{26285
{{lct{8,wa{13,arndm(xl){{set num of dimensions as loop count{26286
{{add{7,xl{13,arofs(xl){{point past bounds information{26287
{{sub{7,xl{19,*arlbd{{set ok offset for proper ptr later{26288
*      loop to stack subscript offsets
{prn17{sub{7,xl{19,*ardms{{point to next set of bounds{26292
{{sti{3,prnsi{{{save current offset{26293
{{rmi{13,ardim(xl){{{get remainder on dividing by dimens{26294
{{mfi{11,-(xs){{{store on stack (one word){26295
{{ldi{3,prnsi{{{reload argument{26296
{{dvi{13,ardim(xl){{{divide to get quotient{26297
{{bct{8,wa{6,prn17{{loop till all stacked{26298
{{zer{7,xr{{{set offset to first set of bounds{26299
{{lct{8,wb{13,arndm(xl){{load count of dims to control loop{26300
{{brn{6,prn19{{{jump into print loop{26301
*      loop to print subscripts from stack adjusting by adding
*      the appropriate low bound value from the arblk
{prn18{mov{8,wa{18,=ch_cm{{load a comma{26306
{{jsr{6,prtch{{{print it{26307
*      merge here first time in (no comma required)
{prn19{mti{10,(xs)+{{{load subscript offset as integer{26311
{{add{7,xl{7,xr{{point to current lbd{26312
{{adi{13,arlbd(xl){{{add lbd to get signed subscript{26313
{{sub{7,xl{7,xr{{point back to start of arblk{26314
{{jsr{6,prtin{{{print subscript{26315
{{add{7,xr{19,*ardms{{bump offset to next bounds{26316
{{bct{8,wb{6,prn18{{loop back till all printed{26317
{{brn{6,prn14{{{merge back to print right bracket{26318
{{enp{{{{end procedure prtnm{26319
{{ejc{{{{{26320
*      prtnv -- print name value
*      prtnv is used by the trace and dump routines to print
*      a line of the form
*      name = value
*      note that the name involved can never be a pseudo-var
*      (xl)                  name base
*      (wa)                  name offset
*      jsr  prtnv            call to print name = value
*      (wb,wc,ra)            destroyed
{prtnv{prc{25,e{1,0{{entry point{26336
{{jsr{6,prtnm{{{print argument name{26337
{{mov{11,-(xs){7,xr{{save entry xr{26338
{{mov{11,-(xs){8,wa{{save name offset (collectable){26339
{{mov{7,xr{21,=tmbeb{{point to blank equal blank{26340
{{jsr{6,prtst{{{print it{26341
{{mov{7,xr{7,xl{{copy name base{26342
{{add{7,xr{8,wa{{point to value{26343
{{mov{7,xr{9,(xr){{load value pointer{26344
{{jsr{6,prtvl{{{print value{26345
{{jsr{6,prtnl{{{terminate line{26346
{{mov{8,wa{10,(xs)+{{restore name offset{26347
{{mov{7,xr{10,(xs)+{{restore entry xr{26348
{{exi{{{{return to caller{26349
{{enp{{{{end procedure prtnv{26350
{{ejc{{{{{26351
*      prtpg  -- print a page throw
*      prints a page throw or a few blank lines on the standard
*      listing channel depending on the listing options chosen.
*      jsr  prtpg            call for page eject
{prtpg{prc{25,e{1,0{{entry point{26360
{{beq{3,stage{18,=stgxt{6,prp01{jump if execution time{26361
{{bze{3,lstlc{6,prp06{{return if top of page already{26362
{{zer{3,lstlc{{{clear line count{26363
*      check type of listing
{prp01{mov{11,-(xs){7,xr{{preserve xr{26367
{{bnz{3,prstd{6,prp02{{eject if flag set{26368
{{bnz{3,prich{6,prp03{{jump if interactive listing channel{26369
{{bze{3,precl{6,prp03{{jump if compact listing{26370
*      perform an eject
{prp02{jsr{6,sysep{{{eject{26374
{{brn{6,prp04{{{merge{26375
*      compact or interactive channel listing. cant print
*      blanks until check made for headers printed and flag set.
{prp03{mov{7,xr{3,headp{{remember headp{26381
{{mnz{3,headp{{{set to avoid repeated prtpg calls{26382
{{jsr{6,prtnl{{{print blank line{26383
{{jsr{6,prtnl{{{print blank line{26384
{{jsr{6,prtnl{{{print blank line{26385
{{mov{3,lstlc{18,=num03{{count blank lines{26386
{{mov{3,headp{7,xr{{restore header flag{26387
{{ejc{{{{{26388
*      prptg (continued)
*      print the heading
{prp04{bnz{3,headp{6,prp05{{jump if header listed{26394
{{mnz{3,headp{{{mark headers printed{26395
{{mov{11,-(xs){7,xl{{keep xl{26396
{{mov{7,xr{21,=headr{{point to listing header{26397
{{jsr{6,prtst{{{place it{26398
{{jsr{6,sysid{{{get system identification{26399
{{jsr{6,prtst{{{append extra chars{26400
{{jsr{6,prtnl{{{print it{26401
{{mov{7,xr{7,xl{{extra header line{26402
{{jsr{6,prtst{{{place it{26403
{{jsr{6,prtnl{{{print it{26404
{{jsr{6,prtnl{{{print a blank{26405
{{jsr{6,prtnl{{{and another{26406
{{add{3,lstlc{18,=num04{{four header lines printed{26407
{{mov{7,xl{10,(xs)+{{restore xl{26408
*      merge if header not printed
{prp05{mov{7,xr{10,(xs)+{{restore xr{26412
*      return
{prp06{exi{{{{return{26416
{{enp{{{{end procedure prtpg{26417
{{ejc{{{{{26418
*      prtps - print page with test for standard listing option
*      if the standard listing option is selected, insist that
*      an eject be done
*      jsr  prtps            call for eject
{prtps{prc{25,e{1,0{{entry point{26427
{{mov{3,prstd{3,prsto{{copy option flag{26428
{{jsr{6,prtpg{{{print page{26429
{{zer{3,prstd{{{clear flag{26430
{{exi{{{{return{26431
{{enp{{{{end procedure prtps{26432
{{ejc{{{{{26433
*      prtsn -- print statement number
*      prtsn is used to initiate a print trace line by printing
*      asterisks and the current statement number. the actual
*      format of the output generated is.
*      ***nnnnn**** iii.....iiii
*      nnnnn is the statement number with leading zeros replaced
*      by asterisks (e.g. *******9****)
*      iii...iii represents a variable length output consisting
*      of a number of letter i characters equal to fnclevel.
*      jsr  prtsn            call to print statement number
*      (wc)                  destroyed
{prtsn{prc{25,e{1,0{{entry point{26452
{{mov{11,-(xs){7,xr{{save entry xr{26453
{{mov{3,prsna{8,wa{{save entry wa{26454
{{mov{7,xr{21,=tmasb{{point to asterisks{26455
{{jsr{6,prtst{{{print asterisks{26456
{{mov{3,profs{18,=num04{{point into middle of asterisks{26457
{{mti{3,kvstn{{{load statement number as integer{26458
{{jsr{6,prtin{{{print integer statement number{26459
{{mov{3,profs{18,=prsnf{{point past asterisks plus blank{26460
{{mov{7,xr{3,kvfnc{{get fnclevel{26461
{{mov{8,wa{18,=ch_li{{set letter i{26462
*      loop to generate letter i fnclevel times
{prsn1{bze{7,xr{6,prsn2{{jump if all set{26466
{{jsr{6,prtch{{{else print an i{26467
{{dcv{7,xr{{{decrement counter{26468
{{brn{6,prsn1{{{loop back{26469
*      merge with all letter i characters generated
{prsn2{mov{8,wa{18,=ch_bl{{get blank{26473
{{jsr{6,prtch{{{print blank{26474
{{mov{8,wa{3,prsna{{restore entry wa{26475
{{mov{7,xr{10,(xs)+{{restore entry xr{26476
{{exi{{{{return to prtsn caller{26477
{{enp{{{{end procedure prtsn{26478
{{ejc{{{{{26479
*      prtst -- print string
*      prtst places a string of characters in the print buffer
*      see prtnl for global locations used
*      note that the first word of the block (normally b_scl)
*      is not used and need not be set correctly (see prtvn)
*      (xr)                  string to be printed
*      jsr  prtst            call to print string
*      (profs)               updated past chars placed
{prtst{prc{25,r{1,0{{entry point{26494
{{bnz{3,headp{6,prst0{{were headers printed{26495
{{jsr{6,prtps{{{no - print them{26496
*      call syspr
{prst0{mov{3,prsva{8,wa{{save wa{26500
{{mov{3,prsvb{8,wb{{save wb{26501
{{zer{8,wb{{{set chars printed count to zero{26502
*      loop to print successive lines for long string
{prst1{mov{8,wa{13,sclen(xr){{load string length{26506
{{sub{8,wa{8,wb{{subtract count of chars already out{26507
{{bze{8,wa{6,prst4{{jump to exit if none left{26508
{{mov{11,-(xs){7,xl{{else stack entry xl{26509
{{mov{11,-(xs){7,xr{{save argument{26510
{{mov{7,xl{7,xr{{copy for eventual move{26511
{{mov{7,xr{3,prlen{{load print buffer length{26512
{{sub{7,xr{3,profs{{get chars left in print buffer{26513
{{bnz{7,xr{6,prst2{{skip if room left on this line{26514
{{jsr{6,prtnl{{{else print this line{26515
{{mov{7,xr{3,prlen{{and set full width available{26516
{{ejc{{{{{26517
*      prtst (continued)
*      here with chars to print and some room in buffer
{prst2{blo{8,wa{7,xr{6,prst3{jump if room for rest of string{26523
{{mov{8,wa{7,xr{{else set to fill line{26524
*      merge here with character count in wa
{prst3{mov{7,xr{3,prbuf{{point to print buffer{26528
{{plc{7,xl{8,wb{{point to location in string{26529
{{psc{7,xr{3,profs{{point to location in buffer{26530
{{add{8,wb{8,wa{{bump string chars count{26531
{{add{3,profs{8,wa{{bump buffer pointer{26532
{{mov{3,prsvc{8,wb{{preserve char counter{26533
{{mvc{{{{move characters to buffer{26534
{{mov{8,wb{3,prsvc{{recover char counter{26535
{{mov{7,xr{10,(xs)+{{restore argument pointer{26536
{{mov{7,xl{10,(xs)+{{restore entry xl{26537
{{brn{6,prst1{{{loop back to test for more{26538
*      here to exit after printing string
{prst4{mov{8,wb{3,prsvb{{restore entry wb{26542
{{mov{8,wa{3,prsva{{restore entry wa{26543
{{exi{{{{return to prtst caller{26544
{{enp{{{{end procedure prtst{26545
{{ejc{{{{{26546
*      prttr -- print to terminal
*      called to print contents of standard print buffer to
*      online terminal. clears buffer down and resets profs.
*      jsr  prttr            call for print
*      (wa,wb)               destroyed
{prttr{prc{25,e{1,0{{entry point{26556
{{mov{11,-(xs){7,xr{{save xr{26557
{{jsr{6,prtic{{{print buffer contents{26558
{{mov{7,xr{3,prbuf{{point to print bfr to clear it{26559
{{lct{8,wa{3,prlnw{{get buffer length{26560
{{add{7,xr{19,*schar{{point past scblk header{26561
{{mov{8,wb{4,nullw{{get blanks{26562
*      loop to clear buffer
{prtt1{mov{10,(xr)+{8,wb{{clear a word{26566
{{bct{8,wa{6,prtt1{{loop{26567
{{zer{3,profs{{{reset profs{26568
{{mov{7,xr{10,(xs)+{{restore xr{26569
{{exi{{{{return{26570
{{enp{{{{end procedure prttr{26571
{{ejc{{{{{26572
*      prtvl -- print a value
*      prtvl places an appropriate character representation of
*      a data value in the print buffer for dump/trace use.
*      (xr)                  value to be printed
*      jsr  prtvl            call to print value
*      (wa,wb,wc,ra)         destroyed
{prtvl{prc{25,r{1,0{{entry point, recursive{26583
{{mov{11,-(xs){7,xl{{save entry xl{26584
{{mov{11,-(xs){7,xr{{save argument{26585
{{chk{{{{check for stack overflow{26586
*      loop back here after finding a trap block (trblk)
{prv01{mov{3,prvsi{13,idval(xr){{copy idval (if any){26590
{{mov{7,xl{9,(xr){{load first word of block{26591
{{lei{7,xl{{{load entry point id{26592
{{bsw{7,xl{2,bl__t{6,prv02{switch on block type{26593
{{iff{2,bl_ar{6,prv05{{arblk{26611
{{iff{1,1{6,prv02{{{26611
{{iff{1,2{6,prv02{{{26611
{{iff{2,bl_ic{6,prv08{{icblk{26611
{{iff{2,bl_nm{6,prv09{{nmblk{26611
{{iff{1,5{6,prv02{{{26611
{{iff{1,6{6,prv02{{{26611
{{iff{1,7{6,prv02{{{26611
{{iff{2,bl_rc{6,prv08{{rcblk{26611
{{iff{2,bl_sc{6,prv11{{scblk{26611
{{iff{2,bl_se{6,prv12{{seblk{26611
{{iff{2,bl_tb{6,prv13{{tbblk{26611
{{iff{2,bl_vc{6,prv13{{vcblk{26611
{{iff{1,13{6,prv02{{{26611
{{iff{1,14{6,prv02{{{26611
{{iff{1,15{6,prv02{{{26611
{{iff{2,bl_pd{6,prv10{{pdblk{26611
{{iff{2,bl_tr{6,prv04{{trblk{26611
{{esw{{{{end of switch on block type{26611
*      here for blocks for which we just print datatype name
{prv02{jsr{6,dtype{{{get datatype name{26615
{{jsr{6,prtst{{{print datatype name{26616
*      common exit point
{prv03{mov{7,xr{10,(xs)+{{reload argument{26620
{{mov{7,xl{10,(xs)+{{restore xl{26621
{{exi{{{{return to prtvl caller{26622
*      here for trblk
{prv04{mov{7,xr{13,trval(xr){{load real value{26626
{{brn{6,prv01{{{and loop back{26627
{{ejc{{{{{26628
*      prtvl (continued)
*      here for array (arblk)
*      print array ( prototype ) blank number idval
{prv05{mov{7,xl{7,xr{{preserve argument{26636
{{mov{7,xr{21,=scarr{{point to datatype name (array){26637
{{jsr{6,prtst{{{print it{26638
{{mov{8,wa{18,=ch_pp{{load left paren{26639
{{jsr{6,prtch{{{print left paren{26640
{{add{7,xl{13,arofs(xl){{point to prototype{26641
{{mov{7,xr{9,(xl){{load prototype{26642
{{jsr{6,prtst{{{print prototype{26643
*      vcblk, tbblk, bcblk merge here for ) blank number idval
{prv06{mov{8,wa{18,=ch_rp{{load right paren{26647
{{jsr{6,prtch{{{print right paren{26648
*      pdblk merges here to print blank number idval
{prv07{mov{8,wa{18,=ch_bl{{load blank{26652
{{jsr{6,prtch{{{print it{26653
{{mov{8,wa{18,=ch_nm{{load number sign{26654
{{jsr{6,prtch{{{print it{26655
{{mti{3,prvsi{{{get idval{26656
{{jsr{6,prtin{{{print id number{26657
{{brn{6,prv03{{{back to exit{26658
*      here for integer (icblk), real (rcblk)
*      print character representation of value
{prv08{mov{11,-(xs){7,xr{{stack argument for gtstg{26664
{{jsr{6,gtstg{{{convert to string{26665
{{ppm{{{{error return is impossible{26666
{{jsr{6,prtst{{{print the string{26667
{{mov{3,dnamp{7,xr{{delete garbage string from storage{26668
{{brn{6,prv03{{{back to exit{26669
{{ejc{{{{{26670
*      prtvl (continued)
*      name (nmblk)
*      for pseudo-variable, just print datatype name (name)
*      for all other names, print dot followed by name rep
{prv09{mov{7,xl{13,nmbas(xr){{load name base{26679
{{mov{8,wa{9,(xl){{load first word of block{26680
{{beq{8,wa{22,=b_kvt{6,prv02{just print name if keyword{26681
{{beq{8,wa{22,=b_evt{6,prv02{just print name if expression var{26682
{{mov{8,wa{18,=ch_dt{{else get dot{26683
{{jsr{6,prtch{{{and print it{26684
{{mov{8,wa{13,nmofs(xr){{load name offset{26685
{{jsr{6,prtnm{{{print name{26686
{{brn{6,prv03{{{back to exit{26687
*      program datatype (pdblk)
*      print datatype name ch_bl ch_nm idval
{prv10{jsr{6,dtype{{{get datatype name{26693
{{jsr{6,prtst{{{print datatype name{26694
{{brn{6,prv07{{{merge back to print id{26695
*      here for string (scblk)
*      print quote string-characters quote
{prv11{mov{8,wa{18,=ch_sq{{load single quote{26701
{{jsr{6,prtch{{{print quote{26702
{{jsr{6,prtst{{{print string value{26703
{{jsr{6,prtch{{{print another quote{26704
{{brn{6,prv03{{{back to exit{26705
{{ejc{{{{{26706
*      prtvl (continued)
*      here for simple expression (seblk)
*      print asterisk variable-name
{prv12{mov{8,wa{18,=ch_as{{load asterisk{26714
{{jsr{6,prtch{{{print asterisk{26715
{{mov{7,xr{13,sevar(xr){{load variable pointer{26716
{{jsr{6,prtvn{{{print variable name{26717
{{brn{6,prv03{{{jump back to exit{26718
*      here for table (tbblk) and array (vcblk)
*      print datatype ( prototype ) blank number idval
{prv13{mov{7,xl{7,xr{{preserve argument{26724
{{jsr{6,dtype{{{get datatype name{26725
{{jsr{6,prtst{{{print datatype name{26726
{{mov{8,wa{18,=ch_pp{{load left paren{26727
{{jsr{6,prtch{{{print left paren{26728
{{mov{8,wa{13,tblen(xl){{load length of block (=vclen){26729
{{btw{8,wa{{{convert to word count{26730
{{sub{8,wa{18,=tbsi_{{allow for standard fields{26731
{{beq{9,(xl){22,=b_tbt{6,prv14{jump if table{26732
{{add{8,wa{18,=vctbd{{for vcblk, adjust size{26733
*      print prototype
{prv14{mti{8,wa{{{move as integer{26737
{{jsr{6,prtin{{{print integer prototype{26738
{{brn{6,prv06{{{merge back for rest{26739
{{enp{{{{end procedure prtvl{26762
{{ejc{{{{{26763
*      prtvn -- print natural variable name
*      prtvn prints the name of a natural variable
*      (xr)                  pointer to vrblk
*      jsr  prtvn            call to print variable name
{prtvn{prc{25,e{1,0{{entry point{26772
{{mov{11,-(xs){7,xr{{stack vrblk pointer{26773
{{add{7,xr{19,*vrsof{{point to possible string name{26774
{{bnz{13,sclen(xr){6,prvn1{{jump if not system variable{26775
{{mov{7,xr{13,vrsvo(xr){{point to svblk with name{26776
*      merge here with dummy scblk pointer in xr
{prvn1{jsr{6,prtst{{{print string name of variable{26780
{{mov{7,xr{10,(xs)+{{restore vrblk pointer{26781
{{exi{{{{return to prtvn caller{26782
{{enp{{{{end procedure prtvn{26783
{{ejc{{{{{26786
*      rcbld -- build a real block
*      (ra)                  real value for rcblk
*      jsr  rcbld            call to build real block
*      (xr)                  pointer to result rcblk
*      (wa)                  destroyed
{rcbld{prc{25,e{1,0{{entry point{26795
{{mov{7,xr{3,dnamp{{load pointer to next available loc{26796
{{add{7,xr{19,*rcsi_{{point past new rcblk{26797
{{blo{7,xr{3,dname{6,rcbl1{jump if there is room{26798
{{mov{8,wa{19,*rcsi_{{else load rcblk length{26799
{{jsr{6,alloc{{{use standard allocator to get block{26800
{{add{7,xr{8,wa{{point past block to merge{26801
*      merge here with xr pointing past the block obtained
{rcbl1{mov{3,dnamp{7,xr{{set new pointer{26805
{{sub{7,xr{19,*rcsi_{{point back to start of block{26806
{{mov{9,(xr){22,=b_rcl{{store type word{26807
{{str{13,rcval(xr){{{store real value in rcblk{26808
{{exi{{{{return to rcbld caller{26809
{{enp{{{{end procedure rcbld{26810
{{ejc{{{{{26812
*      readr -- read next source image at compile time
*      readr is used to read the next source image. to process
*      continuation cards properly, the compiler must read one
*      line ahead. thus readr does not destroy the current image
*      see also the nexts routine which actually gets the image.
*      jsr  readr            call to read next image
*      (xr)                  ptr to next image (0 if none)
*      (r_cni)               copy of pointer
*      (wa,wb,wc,xl)         destroyed
{readr{prc{25,e{1,0{{entry point{26826
{{mov{7,xr{3,r_cni{{get ptr to next image{26827
{{bnz{7,xr{6,read3{{exit if already read{26828
{{bnz{3,cnind{6,reada{{if within include file{26830
{{bne{3,stage{18,=stgic{6,read3{exit if not initial compile{26832
{reada{mov{8,wa{3,cswin{{max read length{26833
{{zer{7,xl{{{clear any dud value in xl{26834
{{jsr{6,alocs{{{allocate buffer{26835
{{jsr{6,sysrd{{{read input image{26836
{{ppm{6,read4{{{jump if eof or new file name{26837
{{icv{3,rdnln{{{increment next line number{26838
{{dcv{3,polct{{{test if time to poll interface{26840
{{bnz{3,polct{6,read0{{not yet{26841
{{zer{8,wa{{{=0 for poll{26842
{{mov{8,wb{3,rdnln{{line number{26843
{{jsr{6,syspl{{{allow interactive access{26844
{{err{1,320{26,user interrupt{{{26845
{{ppm{{{{single step{26846
{{ppm{{{{expression evaluation{26847
{{mov{3,polcs{8,wa{{new countdown start value{26848
{{mov{3,polct{8,wa{{new counter value{26849
{read0{ble{13,sclen(xr){3,cswin{6,read1{use smaller of string lnth ...{26851
{{mov{13,sclen(xr){3,cswin{{... and xxx of -inxxx{26852
*      perform the trim
{read1{mnz{8,wb{{{set trimr to perform trim{26856
{{jsr{6,trimr{{{trim trailing blanks{26857
*      merge here after read
{read2{mov{3,r_cni{7,xr{{store copy of pointer{26861
*      merge here if no read attempted
{read3{exi{{{{return to readr caller{26865
*      here on end of file or new source file name.
*      if this is a new source file name, the r_sfn table will
*      be augmented with a new table entry consisting of the
*      current compiler statement number as subscript, and the
*      file name as value.
{read4{bze{13,sclen(xr){6,read5{{jump if true end of file{26874
{{zer{8,wb{{{new source file name{26875
{{mov{3,rdnln{8,wb{{restart line counter for new file{26876
{{jsr{6,trimr{{{remove unused space in block{26877
{{jsr{6,newfn{{{record new file name{26878
{{brn{6,reada{{{now reissue read for record data{26879
*      here on end of file
{read5{mov{3,dnamp{7,xr{{pop unused scblk{26883
{{bze{3,cnind{6,read6{{jump if not within an include file{26885
{{zer{7,xl{{{eof within include file{26886
{{jsr{6,sysif{{{switch stream back to previous file{26887
{{ppm{{{{{26888
{{mov{8,wa{3,cnind{{restore prev line number, file name{26889
{{add{8,wa{18,=vcvlb{{vector offset in words{26890
{{wtb{8,wa{{{convert to bytes{26891
{{mov{7,xr{3,r_ifa{{file name array{26892
{{add{7,xr{8,wa{{ptr to element{26893
{{mov{3,r_sfc{9,(xr){{change source file name{26894
{{mov{9,(xr){21,=nulls{{release scblk{26895
{{mov{7,xr{3,r_ifl{{line number array{26896
{{add{7,xr{8,wa{{ptr to element{26897
{{mov{7,xl{9,(xr){{icblk containing saved line number{26898
{{ldi{13,icval(xl){{{line number integer{26899
{{mfi{3,rdnln{{{change source line number{26900
{{mov{9,(xr){21,=inton{{release icblk{26901
{{dcv{3,cnind{{{decrement nesting level{26902
{{mov{8,wb{3,cmpsn{{current statement number{26903
{{icv{8,wb{{{anticipate end of previous stmt{26904
{{mti{8,wb{{{convert to integer{26905
{{jsr{6,icbld{{{build icblk for stmt number{26906
{{mov{7,xl{3,r_sfn{{file name table{26907
{{mnz{8,wb{{{lookup statement number by name{26908
{{jsr{6,tfind{{{allocate new teblk{26909
{{ppm{{{{always possible to allocate block{26910
{{mov{13,teval(xl){3,r_sfc{{record file name as entry value{26911
{{beq{3,stage{18,=stgic{6,reada{if initial compile, reissue read{26912
{{bnz{3,cnind{6,reada{{still reading from include file{26913
*      outer nesting of execute-time compile of -include
*      resume with any string remaining prior to -include.
{{mov{7,xl{3,r_ici{{restore code argument string{26918
{{zer{3,r_ici{{{release original string{26919
{{mov{8,wa{3,cnsil{{get length of string{26920
{{mov{8,wb{3,cnspt{{offset of characters left{26921
{{sub{8,wa{8,wb{{number of characters left{26922
{{mov{3,scnil{8,wa{{set new scan length{26923
{{zer{3,scnpt{{{scan from start of substring{26924
{{jsr{6,sbstr{{{create substring of remainder{26925
{{mov{3,r_cim{7,xr{{set scan image{26926
{{brn{6,read2{{{return{26927
{read6{zer{7,xr{{{zero ptr as result{26943
{{brn{6,read2{{{merge{26944
{{enp{{{{end procedure readr{26945
{{ejc{{{{{26946
*      sbstr -- build a substring
*      (xl)                  ptr to scblk/bfblk with chars
*      (wa)                  number of chars in substring
*      (wb)                  offset to first char in scblk
*      jsr  sbstr            call to build substring
*      (xr)                  ptr to new scblk with substring
*      (xl)                  zero
*      (wa,wb,wc,xl,ia)      destroyed
*      note that sbstr is called with a dummy string pointer
*      (pointing into a vrblk or svblk) to copy the name of a
*      variable as a standard string value.
{sbstr{prc{25,e{1,0{{entry point{27041
{{bze{8,wa{6,sbst2{{jump if null substring{27042
{{jsr{6,alocs{{{else allocate scblk{27043
{{mov{8,wa{8,wc{{move number of characters{27044
{{mov{8,wc{7,xr{{save ptr to new scblk{27045
{{plc{7,xl{8,wb{{prepare to load chars from old blk{27046
{{psc{7,xr{{{prepare to store chars in new blk{27047
{{mvc{{{{move characters to new string{27048
{{mov{7,xr{8,wc{{then restore scblk pointer{27049
*      return point
{sbst1{zer{7,xl{{{clear garbage pointer in xl{27053
{{exi{{{{return to sbstr caller{27054
*      here for null substring
{sbst2{mov{7,xr{21,=nulls{{set null string as result{27058
{{brn{6,sbst1{{{return{27059
{{enp{{{{end procedure sbstr{27060
{{ejc{{{{{27061
*      stgcc -- compute counters for stmt startup testing
*      jsr  stgcc            call to recompute counters
*      (wa,wb)               destroyed
*      on exit, stmcs and stmct contain the counter value to
*      tested in stmgo.
{stgcc{prc{25,e{1,0{{{27072
{{mov{8,wa{3,polcs{{assume no profiling or stcount tracing{27074
{{mov{8,wb{18,=num01{{poll each time polcs expires{27075
{{ldi{3,kvstl{{{get stmt limit{27079
{{bnz{3,kvpfl{6,stgc1{{jump if profiling enabled{27080
{{ilt{6,stgc3{{{no stcount tracing if negative{27081
{{bze{3,r_stc{6,stgc2{{jump if not stcount tracing{27082
*      here if profiling or if stcount tracing enabled
{stgc1{mov{8,wb{8,wa{{count polcs times within stmg{27087
{{mov{8,wa{18,=num01{{break out of stmgo on each stmt{27088
{{brn{6,stgc3{{{{27092
*      check that stmcs does not exceed kvstl
{stgc2{mti{8,wa{{{breakout count start value{27096
{{sbi{3,kvstl{{{proposed stmcs minus stmt limit{27097
{{ile{6,stgc3{{{jump if stmt count does not limit{27098
{{ldi{3,kvstl{{{stlimit limits breakcount count{27099
{{mfi{8,wa{{{use it instead{27100
*      re-initialize counter
{stgc3{mov{3,stmcs{8,wa{{update breakout count start value{27104
{{mov{3,stmct{8,wa{{reset breakout counter{27105
{{mov{3,polct{8,wb{{{27107
{{exi{{{{{27109
{{ejc{{{{{27110
*      tfind -- locate table element
*      (xr)                  subscript value for element
*      (xl)                  pointer to table
*      (wb)                  zero by value, non-zero by name
*      jsr  tfind            call to locate element
*      ppm  loc              transfer location if access fails
*      (xr)                  element value (if by value)
*      (xr)                  destroyed (if by name)
*      (xl,wa)               teblk name (if by name)
*      (xl,wa)               destroyed (if by value)
*      (wc,ra)               destroyed
*      note that if a call by value specifies a non-existent
*      subscript, the default value is returned without building
*      a new teblk.
{tfind{prc{25,e{1,1{{entry point{27129
{{mov{11,-(xs){8,wb{{save name/value indicator{27130
{{mov{11,-(xs){7,xr{{save subscript value{27131
{{mov{11,-(xs){7,xl{{save table pointer{27132
{{mov{8,wa{13,tblen(xl){{load length of tbblk{27133
{{btw{8,wa{{{convert to word count{27134
{{sub{8,wa{18,=tbbuk{{get number of buckets{27135
{{mti{8,wa{{{convert to integer value{27136
{{sti{3,tfnsi{{{save for later{27137
{{mov{7,xl{9,(xr){{load first word of subscript{27138
{{lei{7,xl{{{load block entry id (bl_xx){27139
{{bsw{7,xl{2,bl__d{6,tfn00{switch on block type{27140
{{iff{1,0{6,tfn00{{{27151
{{iff{1,1{6,tfn00{{{27151
{{iff{1,2{6,tfn00{{{27151
{{iff{2,bl_ic{6,tfn02{{jump if integer{27151
{{iff{2,bl_nm{6,tfn04{{jump if name{27151
{{iff{2,bl_p0{6,tfn03{{jump if pattern{27151
{{iff{2,bl_p1{6,tfn03{{jump if pattern{27151
{{iff{2,bl_p2{6,tfn03{{jump if pattern{27151
{{iff{2,bl_rc{6,tfn02{{real{27151
{{iff{2,bl_sc{6,tfn05{{jump if string{27151
{{iff{1,10{6,tfn00{{{27151
{{iff{1,11{6,tfn00{{{27151
{{iff{1,12{6,tfn00{{{27151
{{iff{1,13{6,tfn00{{{27151
{{iff{1,14{6,tfn00{{{27151
{{iff{1,15{6,tfn00{{{27151
{{iff{1,16{6,tfn00{{{27151
{{esw{{{{end switch on block type{27151
*      here for blocks for which we use the second word of the
*      block as the hash source (see block formats for details).
{tfn00{mov{8,wa{12,1(xr){{load second word{27156
*      merge here with one word hash source in wa
{tfn01{mti{8,wa{{{convert to integer{27160
{{brn{6,tfn06{{{jump to merge{27161
{{ejc{{{{{27162
*      tfind (continued)
*      here for integer or real
*      possibility of overflow exist on twos complement
*      machine if hash source is most negative integer or is
*      a real having the same bit pattern.
{tfn02{ldi{12,1(xr){{{load value as hash source{27172
{{ige{6,tfn06{{{ok if positive or zero{27173
{{ngi{{{{make positive{27174
{{iov{6,tfn06{{{clear possible overflow{27175
{{brn{6,tfn06{{{merge{27176
*      for pattern, use first word (pcode) as source
{tfn03{mov{8,wa{9,(xr){{load first word as hash source{27180
{{brn{6,tfn01{{{merge back{27181
*      for name, use offset as hash source
{tfn04{mov{8,wa{13,nmofs(xr){{load offset as hash source{27185
{{brn{6,tfn01{{{merge back{27186
*      here for string
{tfn05{jsr{6,hashs{{{call routine to compute hash{27190
*      merge here with hash source in (ia)
{tfn06{rmi{3,tfnsi{{{compute hash index by remaindering{27194
{{mfi{8,wc{{{get as one word integer{27195
{{wtb{8,wc{{{convert to byte offset{27196
{{mov{7,xl{9,(xs){{get table ptr again{27197
{{add{7,xl{8,wc{{point to proper bucket{27198
{{mov{7,xr{13,tbbuk(xl){{load first teblk pointer{27199
{{beq{7,xr{9,(xs){6,tfn10{jump if no teblks on chain{27200
*      loop through teblks on hash chain
{tfn07{mov{8,wb{7,xr{{save teblk pointer{27204
{{mov{7,xr{13,tesub(xr){{load subscript value{27205
{{mov{7,xl{12,1(xs){{load input argument subscript val{27206
{{jsr{6,ident{{{compare them{27207
{{ppm{6,tfn08{{{jump if equal (ident){27208
*      here if no match with that teblk
{{mov{7,xl{8,wb{{restore teblk pointer{27212
{{mov{7,xr{13,tenxt(xl){{point to next teblk on chain{27213
{{bne{7,xr{9,(xs){6,tfn07{jump if there is one{27214
*      here if no match with any teblk on chain
{{mov{8,wc{19,*tenxt{{set offset to link field (xl base){27218
{{brn{6,tfn11{{{jump to merge{27219
{{ejc{{{{{27220
*      tfind (continued)
*      here we have found a matching element
{tfn08{mov{7,xl{8,wb{{restore teblk pointer{27226
{{mov{8,wa{19,*teval{{set teblk name offset{27227
{{mov{8,wb{12,2(xs){{restore name/value indicator{27228
{{bnz{8,wb{6,tfn09{{jump if called by name{27229
{{jsr{6,acess{{{else get value{27230
{{ppm{6,tfn12{{{jump if reference fails{27231
{{zer{8,wb{{{restore name/value indicator{27232
*      common exit for entry found
{tfn09{add{7,xs{19,*num03{{pop stack entries{27236
{{exi{{{{return to tfind caller{27237
*      here if no teblks on the hash chain
{tfn10{add{8,wc{19,*tbbuk{{get offset to bucket ptr{27241
{{mov{7,xl{9,(xs){{set tbblk ptr as base{27242
*      merge here with (xl,wc) base,offset of final link
{tfn11{mov{7,xr{9,(xs){{tbblk pointer{27246
{{mov{7,xr{13,tbinv(xr){{load default value in case{27247
{{mov{8,wb{12,2(xs){{load name/value indicator{27248
{{bze{8,wb{6,tfn09{{exit with default if value call{27249
{{mov{8,wb{7,xr{{copy default value{27250
*      here we must build a new teblk
{{mov{8,wa{19,*tesi_{{set size of teblk{27254
{{jsr{6,alloc{{{allocate teblk{27255
{{add{7,xl{8,wc{{point to hash link{27256
{{mov{9,(xl){7,xr{{link new teblk at end of chain{27257
{{mov{9,(xr){22,=b_tet{{store type word{27258
{{mov{13,teval(xr){8,wb{{set default as initial value{27259
{{mov{13,tenxt(xr){10,(xs)+{{set tbblk ptr to mark end of chain{27260
{{mov{13,tesub(xr){10,(xs)+{{store subscript value{27261
{{mov{8,wb{10,(xs)+{{restore name/value indicator{27262
{{mov{7,xl{7,xr{{copy teblk pointer (name base){27263
{{mov{8,wa{19,*teval{{set offset{27264
{{exi{{{{return to caller with new teblk{27265
*      acess fail return
{tfn12{exi{1,1{{{alternative return{27269
{{enp{{{{end procedure tfind{27270
{{ejc{{{{{27271
*      tmake -- make new table
*      (xl)                  initial lookup value
*      (wc)                  number of buckets desired
*      jsr  tmake            call to make new table
*      (xr)                  new table
*      (wa,wb)               destroyed
{tmake{prc{25,e{1,0{{{27281
{{mov{8,wa{8,wc{{copy number of headers{27282
{{add{8,wa{18,=tbsi_{{adjust for standard fields{27283
{{wtb{8,wa{{{convert length to bytes{27284
{{jsr{6,alloc{{{allocate space for tbblk{27285
{{mov{8,wb{7,xr{{copy pointer to tbblk{27286
{{mov{10,(xr)+{22,=b_tbt{{store type word{27287
{{zer{10,(xr)+{{{zero id for the moment{27288
{{mov{10,(xr)+{8,wa{{store length (tblen){27289
{{mov{10,(xr)+{7,xl{{store initial lookup value{27290
{{lct{8,wc{8,wc{{set loop counter (num headers){27291
*      loop to initialize all bucket pointers
{tma01{mov{10,(xr)+{8,wb{{store tbblk ptr in bucket header{27295
{{bct{8,wc{6,tma01{{loop till all stored{27296
{{mov{7,xr{8,wb{{recall pointer to tbblk{27297
{{exi{{{{{27298
{{enp{{{{{27299
{{ejc{{{{{27300
*      vmake -- create a vector
*      (wa)                  number of elements in vector
*      (xl)                  default value for vector elements
*      jsr  vmake            call to create vector
*      ppm  loc              if vector too large
*      (xr)                  pointer to vcblk
*      (wa,wb,wc,xl)         destroyed
{vmake{prc{25,e{1,1{{entry point{27312
{{lct{8,wb{8,wa{{copy elements for loop later on{27313
{{add{8,wa{18,=vcsi_{{add space for standard fields{27314
{{wtb{8,wa{{{convert length to bytes{27315
{{bgt{8,wa{3,mxlen{6,vmak2{fail if too large{27316
{{jsr{6,alloc{{{allocate space for vcblk{27317
{{mov{9,(xr){22,=b_vct{{store type word{27318
{{zer{13,idval(xr){{{initialize idval{27319
{{mov{13,vclen(xr){8,wa{{set length{27320
{{mov{8,wc{7,xl{{copy default value{27321
{{mov{7,xl{7,xr{{copy vcblk pointer{27322
{{add{7,xl{19,*vcvls{{point to first element value{27323
*      loop to set vector elements to default value
{vmak1{mov{10,(xl)+{8,wc{{store one value{27327
{{bct{8,wb{6,vmak1{{loop till all stored{27328
{{exi{{{{success return{27329
*      here if desired vector size too large
{vmak2{exi{1,1{{{fail return{27333
{{enp{{{{{27334
{{ejc{{{{{27335
*      scane -- scan an element
*      scane is called at compile time (by expan ,cmpil,cncrd)
*      to scan one element from the input image.
*      (scncc)               non-zero if called from cncrd
*      jsr  scane            call to scan element
*      (xr)                  result pointer (see below)
*      (xl)                  syntax type code (t_xxx)
*      the following global locations are used.
*      r_cim                 pointer to string block (scblk)
*                            for current input image.
*      r_cni                 pointer to next input image string
*                            pointer (zero if none).
*      r_scp                 save pointer (exit xr) from last
*                            call in case rescan is set.
*      scnbl                 this location is set non-zero on
*                            exit if scane scanned past blanks
*                            before locating the current element
*                            the end of a line counts as blanks.
*      scncc                 cncrd sets this non-zero to scan
*                            control card names and clears it
*                            on return
*      scnil                 length of current input image
*      scngo                 if set non-zero on entry, f and s
*                            are returned as separate syntax
*                            types (not letters) (goto pro-
*                            cessing). scngo is reset on exit.
*      scnpt                 offset to current loc in r_cim
*      scnrs                 if set non-zero on entry, scane
*                            returns the same result as on the
*                            last call (rescan). scnrs is reset
*                            on exit from any call to scane.
*      scntp                 save syntax type from last
*                            call (in case rescan is set).
{{ejc{{{{{27383
*      scane (continued)
*      element scanned       xl        xr
*      ---------------       --        --
*      control card name     0         pointer to scblk for name
*      unary operator        t_uop     ptr to operator dvblk
*      left paren            t_lpr     t_lpr
*      left bracket          t_lbr     t_lbr
*      comma                 t_cma     t_cma
*      function call         t_fnc     ptr to function vrblk
*      variable              t_var     ptr to vrblk
*      string constant       t_con     ptr to scblk
*      integer constant      t_con     ptr to icblk
*      real constant         t_con     ptr to rcblk
*      binary operator       t_bop     ptr to operator dvblk
*      right paren           t_rpr     t_rpr
*      right bracket         t_rbr     t_rbr
*      colon                 t_col     t_col
*      semi-colon            t_smc     t_smc
*      f (scngo ne 0)        t_fgo     t_fgo
*      s (scngo ne 0)        t_sgo     t_sgo
{{ejc{{{{{27428
*      scane (continued)
*      entry point
{scane{prc{25,e{1,0{{entry point{27434
{{zer{3,scnbl{{{reset blanks flag{27435
{{mov{3,scnsa{8,wa{{save wa{27436
{{mov{3,scnsb{8,wb{{save wb{27437
{{mov{3,scnsc{8,wc{{save wc{27438
{{bze{3,scnrs{6,scn03{{jump if no rescan{27439
*      here for rescan request
{{mov{7,xl{3,scntp{{set previous returned scan type{27443
{{mov{7,xr{3,r_scp{{set previous returned pointer{27444
{{zer{3,scnrs{{{reset rescan switch{27445
{{brn{6,scn13{{{jump to exit{27446
*      come here to read new image to test for continuation
{scn01{jsr{6,readr{{{read next image{27450
{{mov{8,wb{19,*dvubs{{set wb for not reading name{27451
{{bze{7,xr{6,scn30{{treat as semi-colon if none{27452
{{plc{7,xr{{{else point to first character{27453
{{lch{8,wc{9,(xr){{load first character{27454
{{beq{8,wc{18,=ch_dt{6,scn02{jump if dot for continuation{27455
{{bne{8,wc{18,=ch_pl{6,scn30{else treat as semicolon unless plus{27456
*      here for continuation line
{scn02{jsr{6,nexts{{{acquire next source image{27460
{{mov{3,scnpt{18,=num01{{set scan pointer past continuation{27461
{{mnz{3,scnbl{{{set blanks flag{27462
{{ejc{{{{{27463
*      scane (continued)
*      merge here to scan next element on current line
{scn03{mov{8,wa{3,scnpt{{load current offset{27469
{{beq{8,wa{3,scnil{6,scn01{check continuation if end{27470
{{mov{7,xl{3,r_cim{{point to current line{27471
{{plc{7,xl{8,wa{{point to current character{27472
{{mov{3,scnse{8,wa{{set start of element location{27473
{{mov{8,wc{21,=opdvs{{point to operator dv list{27474
{{mov{8,wb{19,*dvubs{{set constant for operator circuit{27475
{{brn{6,scn06{{{start scanning{27476
*      loop here to ignore leading blanks and tabs
{scn05{bze{8,wb{6,scn10{{jump if trailing{27480
{{icv{3,scnse{{{increment start of element{27481
{{beq{8,wa{3,scnil{6,scn01{jump if end of image{27482
{{mnz{3,scnbl{{{note blanks seen{27483
*      the following jump is used repeatedly for scanning out
*      the characters of a numeric constant or variable name.
*      the registers are used as follows.
*      (xr)                  scratch
*      (xl)                  ptr to next character
*      (wa)                  current scan offset
*      (wb)                  *dvubs (0 if scanning name,const)
*      (wc)                  =opdvs (0 if scanning constant)
{scn06{lch{7,xr{10,(xl)+{{get next character{27495
{{icv{8,wa{{{bump scan offset{27496
{{mov{3,scnpt{8,wa{{store offset past char scanned{27497
{{bsw{7,xr{2,cfp_u{6,scn07{switch on scanned character{27499
*      switch table for switch on character
{{ejc{{{{{27526
*      scane (continued)
{{ejc{{{{{27582
*      scane (continued)
{{iff{1,0{6,scn07{{{27615
{{iff{1,1{6,scn07{{{27615
{{iff{1,2{6,scn07{{{27615
{{iff{1,3{6,scn07{{{27615
{{iff{1,4{6,scn07{{{27615
{{iff{1,5{6,scn07{{{27615
{{iff{1,6{6,scn07{{{27615
{{iff{1,7{6,scn07{{{27615
{{iff{1,8{6,scn07{{{27615
{{iff{2,ch_ht{6,scn05{{horizontal tab{27615
{{iff{1,10{6,scn07{{{27615
{{iff{1,11{6,scn07{{{27615
{{iff{1,12{6,scn07{{{27615
{{iff{1,13{6,scn07{{{27615
{{iff{1,14{6,scn07{{{27615
{{iff{1,15{6,scn07{{{27615
{{iff{1,16{6,scn07{{{27615
{{iff{1,17{6,scn07{{{27615
{{iff{1,18{6,scn07{{{27615
{{iff{1,19{6,scn07{{{27615
{{iff{1,20{6,scn07{{{27615
{{iff{1,21{6,scn07{{{27615
{{iff{1,22{6,scn07{{{27615
{{iff{1,23{6,scn07{{{27615
{{iff{1,24{6,scn07{{{27615
{{iff{1,25{6,scn07{{{27615
{{iff{1,26{6,scn07{{{27615
{{iff{1,27{6,scn07{{{27615
{{iff{1,28{6,scn07{{{27615
{{iff{1,29{6,scn07{{{27615
{{iff{1,30{6,scn07{{{27615
{{iff{1,31{6,scn07{{{27615
{{iff{2,ch_bl{6,scn05{{blank{27615
{{iff{2,ch_ex{6,scn37{{exclamation mark{27615
{{iff{2,ch_dq{6,scn17{{double quote{27615
{{iff{2,ch_nm{6,scn41{{number sign{27615
{{iff{2,ch_dl{6,scn36{{dollar{27615
{{iff{1,37{6,scn07{{{27615
{{iff{2,ch_am{6,scn44{{ampersand{27615
{{iff{2,ch_sq{6,scn16{{single quote{27615
{{iff{2,ch_pp{6,scn25{{left paren{27615
{{iff{2,ch_rp{6,scn26{{right paren{27615
{{iff{2,ch_as{6,scn49{{asterisk{27615
{{iff{2,ch_pl{6,scn33{{plus{27615
{{iff{2,ch_cm{6,scn31{{comma{27615
{{iff{2,ch_mn{6,scn34{{minus{27615
{{iff{2,ch_dt{6,scn32{{dot{27615
{{iff{2,ch_sl{6,scn40{{slash{27615
{{iff{2,ch_d0{6,scn08{{digit 0{27615
{{iff{2,ch_d1{6,scn08{{digit 1{27615
{{iff{2,ch_d2{6,scn08{{digit 2{27615
{{iff{2,ch_d3{6,scn08{{digit 3{27615
{{iff{2,ch_d4{6,scn08{{digit 4{27615
{{iff{2,ch_d5{6,scn08{{digit 5{27615
{{iff{2,ch_d6{6,scn08{{digit 6{27615
{{iff{2,ch_d7{6,scn08{{digit 7{27615
{{iff{2,ch_d8{6,scn08{{digit 8{27615
{{iff{2,ch_d9{6,scn08{{digit 9{27615
{{iff{2,ch_cl{6,scn29{{colon{27615
{{iff{2,ch_sm{6,scn30{{semi-colon{27615
{{iff{2,ch_bb{6,scn28{{left bracket{27615
{{iff{2,ch_eq{6,scn46{{equal{27615
{{iff{2,ch_rb{6,scn27{{right bracket{27615
{{iff{2,ch_qu{6,scn45{{question mark{27615
{{iff{2,ch_at{6,scn42{{at{27615
{{iff{2,ch_ua{6,scn09{{shifted a{27615
{{iff{2,ch_ub{6,scn09{{shifted b{27615
{{iff{2,ch_uc{6,scn09{{shifted c{27615
{{iff{2,ch_ud{6,scn09{{shifted d{27615
{{iff{2,ch_ue{6,scn09{{shifted e{27615
{{iff{2,ch_uf{6,scn20{{shifted f{27615
{{iff{2,ch_ug{6,scn09{{shifted g{27615
{{iff{2,ch_uh{6,scn09{{shifted h{27615
{{iff{2,ch_ui{6,scn09{{shifted i{27615
{{iff{2,ch_uj{6,scn09{{shifted j{27615
{{iff{2,ch_uk{6,scn09{{shifted k{27615
{{iff{2,ch_ul{6,scn09{{shifted l{27615
{{iff{2,ch_um{6,scn09{{shifted m{27615
{{iff{2,ch_un{6,scn09{{shifted n{27615
{{iff{2,ch_uo{6,scn09{{shifted o{27615
{{iff{2,ch_up{6,scn09{{shifted p{27615
{{iff{2,ch_uq{6,scn09{{shifted q{27615
{{iff{2,ch_ur{6,scn09{{shifted r{27615
{{iff{2,ch_us{6,scn21{{shifted s{27615
{{iff{2,ch_ut{6,scn09{{shifted t{27615
{{iff{2,ch_uu{6,scn09{{shifted u{27615
{{iff{2,ch_uv{6,scn09{{shifted v{27615
{{iff{2,ch_uw{6,scn09{{shifted w{27615
{{iff{2,ch_ux{6,scn09{{shifted x{27615
{{iff{2,ch_uy{6,scn09{{shifted y{27615
{{iff{2,ch_uz{6,scn09{{shifted z{27615
{{iff{2,ch_ob{6,scn28{{left bracket{27615
{{iff{1,92{6,scn07{{{27615
{{iff{2,ch_cb{6,scn27{{right bracket{27615
{{iff{2,ch_pc{6,scn38{{percent{27615
{{iff{2,ch_u_{6,scn24{{underline{27615
{{iff{1,96{6,scn07{{{27615
{{iff{2,ch_la{6,scn09{{letter a{27615
{{iff{2,ch_lb{6,scn09{{letter b{27615
{{iff{2,ch_lc{6,scn09{{letter c{27615
{{iff{2,ch_ld{6,scn09{{letter d{27615
{{iff{2,ch_le{6,scn09{{letter e{27615
{{iff{2,ch_lf{6,scn20{{letter f{27615
{{iff{2,ch_lg{6,scn09{{letter g{27615
{{iff{2,ch_lh{6,scn09{{letter h{27615
{{iff{2,ch_li{6,scn09{{letter i{27615
{{iff{2,ch_lj{6,scn09{{letter j{27615
{{iff{2,ch_lk{6,scn09{{letter k{27615
{{iff{2,ch_ll{6,scn09{{letter l{27615
{{iff{2,ch_lm{6,scn09{{letter m{27615
{{iff{2,ch_ln{6,scn09{{letter n{27615
{{iff{2,ch_lo{6,scn09{{letter o{27615
{{iff{2,ch_lp{6,scn09{{letter p{27615
{{iff{2,ch_lq{6,scn09{{letter q{27615
{{iff{2,ch_lr{6,scn09{{letter r{27615
{{iff{2,ch_ls{6,scn21{{letter s{27615
{{iff{2,ch_lt{6,scn09{{letter t{27615
{{iff{2,ch_lu{6,scn09{{letter u{27615
{{iff{2,ch_lv{6,scn09{{letter v{27615
{{iff{2,ch_lw{6,scn09{{letter w{27615
{{iff{2,ch_lx{6,scn09{{letter x{27615
{{iff{2,ch_ly{6,scn09{{letter y{27615
{{iff{2,ch_l_{6,scn09{{letter z{27615
{{iff{1,123{6,scn07{{{27615
{{iff{2,ch_br{6,scn43{{vertical bar{27615
{{iff{1,125{6,scn07{{{27615
{{iff{2,ch_nt{6,scn35{{not{27615
{{iff{1,127{6,scn07{{{27615
{{esw{{{{end switch on character{27615
*      here for illegal character (underline merges)
{scn07{bze{8,wb{6,scn10{{jump if scanning name or constant{27619
{{erb{1,230{26,syntax error: illegal character{{{27620
{{ejc{{{{{27621
*      scane (continued)
*      here for digits 0-9
{scn08{bze{8,wb{6,scn09{{keep scanning if name/constant{27627
{{zer{8,wc{{{else set flag for scanning constant{27628
*      here for letter. loop here when scanning name/constant
{scn09{beq{8,wa{3,scnil{6,scn11{jump if end of image{27632
{{zer{8,wb{{{set flag for scanning name/const{27633
{{brn{6,scn06{{{merge back to continue scan{27634
*      come here for delimiter ending name or constant
{scn10{dcv{8,wa{{{reset offset to point to delimiter{27638
*      come here after finishing scan of name or constant
{scn11{mov{3,scnpt{8,wa{{store updated scan offset{27642
{{mov{8,wb{3,scnse{{point to start of element{27643
{{sub{8,wa{8,wb{{get number of characters{27644
{{mov{7,xl{3,r_cim{{point to line image{27645
{{bnz{8,wc{6,scn15{{jump if name{27646
*      here after scanning out numeric constant
{{jsr{6,sbstr{{{get string for constant{27650
{{mov{3,dnamp{7,xr{{delete from storage (not needed){27651
{{jsr{6,gtnum{{{convert to numeric{27652
{{ppm{6,scn14{{{jump if conversion failure{27653
*      merge here to exit with constant
{scn12{mov{7,xl{18,=t_con{{set result type of constant{27657
{{ejc{{{{{27658
*      scane (continued)
*      common exit point (xr,xl) set
{scn13{mov{8,wa{3,scnsa{{restore wa{27664
{{mov{8,wb{3,scnsb{{restore wb{27665
{{mov{8,wc{3,scnsc{{restore wc{27666
{{mov{3,r_scp{7,xr{{save xr in case rescan{27667
{{mov{3,scntp{7,xl{{save xl in case rescan{27668
{{zer{3,scngo{{{reset possible goto flag{27669
{{exi{{{{return to scane caller{27670
*      here if conversion error on numeric item
{scn14{erb{1,231{26,syntax error: invalid numeric item{{{27674
*      here after scanning out variable name
{scn15{jsr{6,sbstr{{{build string name of variable{27678
{{bnz{3,scncc{6,scn13{{return if cncrd call{27679
{{jsr{6,gtnvr{{{locate/build vrblk{27680
{{ppm{{{{dummy (unused) error return{27681
{{mov{7,xl{18,=t_var{{set type as variable{27682
{{brn{6,scn13{{{back to exit{27683
*      here for single quote (start of string constant)
{scn16{bze{8,wb{6,scn10{{terminator if scanning name or cnst{27687
{{mov{8,wb{18,=ch_sq{{set terminator as single quote{27688
{{brn{6,scn18{{{merge{27689
*      here for double quote (start of string constant)
{scn17{bze{8,wb{6,scn10{{terminator if scanning name or cnst{27693
{{mov{8,wb{18,=ch_dq{{set double quote terminator, merge{27694
*      loop to scan out string constant
{scn18{beq{8,wa{3,scnil{6,scn19{error if end of image{27698
{{lch{8,wc{10,(xl)+{{else load next character{27699
{{icv{8,wa{{{bump offset{27700
{{bne{8,wc{8,wb{6,scn18{loop back if not terminator{27701
{{ejc{{{{{27702
*      scane (continued)
*      here after scanning out string constant
{{mov{8,wb{3,scnpt{{point to first character{27708
{{mov{3,scnpt{8,wa{{save offset past final quote{27709
{{dcv{8,wa{{{point back past last character{27710
{{sub{8,wa{8,wb{{get number of characters{27711
{{mov{7,xl{3,r_cim{{point to input image{27712
{{jsr{6,sbstr{{{build substring value{27713
{{brn{6,scn12{{{back to exit with constant result{27714
*      here if no matching quote found
{scn19{mov{3,scnpt{8,wa{{set updated scan pointer{27718
{{erb{1,232{26,syntax error: unmatched string quote{{{27719
*      here for f (possible failure goto)
{scn20{mov{7,xr{18,=t_fgo{{set return code for fail goto{27723
{{brn{6,scn22{{{jump to merge{27724
*      here for s (possible success goto)
{scn21{mov{7,xr{18,=t_sgo{{set success goto as return code{27728
*      special goto cases merge here
{scn22{bze{3,scngo{6,scn09{{treat as normal letter if not goto{27732
*      merge here for special character exit
{scn23{bze{8,wb{6,scn10{{jump if end of name/constant{27736
{{mov{7,xl{7,xr{{else copy code{27737
{{brn{6,scn13{{{and jump to exit{27738
*      here for underline
{scn24{bze{8,wb{6,scn09{{part of name if scanning name{27742
{{brn{6,scn07{{{else illegal{27743
{{ejc{{{{{27744
*      scane (continued)
*      here for left paren
{scn25{mov{7,xr{18,=t_lpr{{set left paren return code{27750
{{bnz{8,wb{6,scn23{{return left paren unless name{27751
{{bze{8,wc{6,scn10{{delimiter if scanning constant{27752
*      here for left paren after name (function call)
{{mov{8,wb{3,scnse{{point to start of name{27756
{{mov{3,scnpt{8,wa{{set pointer past left paren{27757
{{dcv{8,wa{{{point back past last char of name{27758
{{sub{8,wa{8,wb{{get name length{27759
{{mov{7,xl{3,r_cim{{point to input image{27760
{{jsr{6,sbstr{{{get string name for function{27761
{{jsr{6,gtnvr{{{locate/build vrblk{27762
{{ppm{{{{dummy (unused) error return{27763
{{mov{7,xl{18,=t_fnc{{set code for function call{27764
{{brn{6,scn13{{{back to exit{27765
*      processing for special characters
{scn26{mov{7,xr{18,=t_rpr{{right paren, set code{27769
{{brn{6,scn23{{{take special character exit{27770
{scn27{mov{7,xr{18,=t_rbr{{right bracket, set code{27772
{{brn{6,scn23{{{take special character exit{27773
{scn28{mov{7,xr{18,=t_lbr{{left bracket, set code{27775
{{brn{6,scn23{{{take special character exit{27776
{scn29{mov{7,xr{18,=t_col{{colon, set code{27778
{{brn{6,scn23{{{take special character exit{27779
{scn30{mov{7,xr{18,=t_smc{{semi-colon, set code{27781
{{brn{6,scn23{{{take special character exit{27782
{scn31{mov{7,xr{18,=t_cma{{comma, set code{27784
{{brn{6,scn23{{{take special character exit{27785
{{ejc{{{{{27786
*      scane (continued)
*      here for operators. on entry, wc points to the table of
*      operator dope vectors and wb is the increment to step
*      to the next pair (binary/unary) of dope vectors in the
*      list. on reaching scn46, the pointer has been adjusted to
*      point to the appropriate pair of dope vectors.
*      the first three entries are special since they can occur
*      as part of a variable name (.) or constant (.+-).
{scn32{bze{8,wb{6,scn09{{dot can be part of name or constant{27798
{{add{8,wc{8,wb{{else bump pointer{27799
{scn33{bze{8,wc{6,scn09{{plus can be part of constant{27801
{{bze{8,wb{6,scn48{{plus cannot be part of name{27802
{{add{8,wc{8,wb{{else bump pointer{27803
{scn34{bze{8,wc{6,scn09{{minus can be part of constant{27805
{{bze{8,wb{6,scn48{{minus cannot be part of name{27806
{{add{8,wc{8,wb{{else bump pointer{27807
{scn35{add{8,wc{8,wb{{not{27809
{scn36{add{8,wc{8,wb{{dollar{27810
{scn37{add{8,wc{8,wb{{exclamation{27811
{scn38{add{8,wc{8,wb{{percent{27812
{scn39{add{8,wc{8,wb{{asterisk{27813
{scn40{add{8,wc{8,wb{{slash{27814
{scn41{add{8,wc{8,wb{{number sign{27815
{scn42{add{8,wc{8,wb{{at sign{27816
{scn43{add{8,wc{8,wb{{vertical bar{27817
{scn44{add{8,wc{8,wb{{ampersand{27818
{scn45{add{8,wc{8,wb{{question mark{27819
*      all operators come here (equal merges directly)
*      (wc) points to the binary/unary pair of operator dvblks.
{scn46{bze{8,wb{6,scn10{{operator terminates name/constant{27824
{{mov{7,xr{8,wc{{else copy dv pointer{27825
{{lch{8,wc{9,(xl){{load next character{27826
{{mov{7,xl{18,=t_bop{{set binary op in case{27827
{{beq{8,wa{3,scnil{6,scn47{should be binary if image end{27828
{{beq{8,wc{18,=ch_bl{6,scn47{should be binary if followed by blk{27829
{{beq{8,wc{18,=ch_ht{6,scn47{jump if horizontal tab{27831
{{beq{8,wc{18,=ch_sm{6,scn47{semicolon can immediately follow ={27836
{{beq{8,wc{18,=ch_cl{6,scn47{colon can immediately follow ={27837
{{beq{8,wc{18,=ch_rp{6,scn47{right paren can immediately follow ={27838
{{beq{8,wc{18,=ch_rb{6,scn47{right bracket can immediately follow ={27839
{{beq{8,wc{18,=ch_cb{6,scn47{right bracket can immediately follow ={27840
*      here for unary operator
{{add{7,xr{19,*dvbs_{{point to dv for unary op{27844
{{mov{7,xl{18,=t_uop{{set type for unary operator{27845
{{ble{3,scntp{18,=t_uok{6,scn13{ok unary if ok preceding element{27846
{{ejc{{{{{27847
*      scane (continued)
*      merge here to require preceding blanks
{scn47{bnz{3,scnbl{6,scn13{{all ok if preceding blanks, exit{27853
*      fail operator in this position
{scn48{erb{1,233{26,syntax error: invalid use of operator{{{27857
*      here for asterisk, could be ** substitute for exclamation
{scn49{bze{8,wb{6,scn10{{end of name if scanning name{27861
{{beq{8,wa{3,scnil{6,scn39{not ** if * at image end{27862
{{mov{7,xr{8,wa{{else save offset past first *{27863
{{mov{3,scnof{8,wa{{save another copy{27864
{{lch{8,wa{10,(xl)+{{load next character{27865
{{bne{8,wa{18,=ch_as{6,scn50{not ** if next char not *{27866
{{icv{7,xr{{{else step offset past second *{27867
{{beq{7,xr{3,scnil{6,scn51{ok exclam if end of image{27868
{{lch{8,wa{9,(xl){{else load next character{27869
{{beq{8,wa{18,=ch_bl{6,scn51{exclamation if blank{27870
{{beq{8,wa{18,=ch_ht{6,scn51{exclamation if horizontal tab{27872
*      unary *
{scn50{mov{8,wa{3,scnof{{recover stored offset{27880
{{mov{7,xl{3,r_cim{{point to line again{27881
{{plc{7,xl{8,wa{{point to current char{27882
{{brn{6,scn39{{{merge with unary *{27883
*      here for ** as substitute for exclamation
{scn51{mov{3,scnpt{7,xr{{save scan pointer past 2nd *{27887
{{mov{8,wa{7,xr{{copy scan pointer{27888
{{brn{6,scn37{{{merge with exclamation{27889
{{enp{{{{end procedure scane{27890
{{ejc{{{{{27891
*      scngf -- scan goto field
*      scngf is called from cmpil to scan and analyze a goto
*      field including the surrounding brackets or parentheses.
*      for a normal goto, the result returned is either a vrblk
*      pointer for a simple label operand, or a pointer to an
*      expression tree with a special outer unary operator
*      (o_goc). for a direct goto, the result returned is a
*      pointer to an expression tree with the special outer
*      unary operator o_god.
*      jsr  scngf            call to scan goto field
*      (xr)                  result (see above)
*      (xl,wa,wb,wc)         destroyed
{scngf{prc{25,e{1,0{{entry point{27908
{{jsr{6,scane{{{scan initial element{27909
{{beq{7,xl{18,=t_lpr{6,scng1{skip if left paren (normal goto){27910
{{beq{7,xl{18,=t_lbr{6,scng2{skip if left bracket (direct goto){27911
{{erb{1,234{26,syntax error: goto field incorrect{{{27912
*      here for left paren (normal goto)
{scng1{mov{8,wb{18,=num01{{set expan flag for normal goto{27916
{{jsr{6,expan{{{analyze goto field{27917
{{mov{8,wa{21,=opdvn{{point to opdv for complex goto{27918
{{ble{7,xr{3,statb{6,scng3{jump if not in static (sgd15){27919
{{blo{7,xr{3,state{6,scng4{jump to exit if simple label name{27920
{{brn{6,scng3{{{complex goto - merge{27921
*      here for left bracket (direct goto)
{scng2{mov{8,wb{18,=num02{{set expan flag for direct goto{27925
{{jsr{6,expan{{{scan goto field{27926
{{mov{8,wa{21,=opdvd{{set opdv pointer for direct goto{27927
{{ejc{{{{{27928
*      scngf (continued)
*      merge here to build outer unary operator block
{scng3{mov{11,-(xs){8,wa{{stack operator dv pointer{27934
{{mov{11,-(xs){7,xr{{stack pointer to expression tree{27935
{{jsr{6,expop{{{pop operator off{27936
{{mov{7,xr{10,(xs)+{{reload new expression tree pointer{27937
*      common exit point
{scng4{exi{{{{return to caller{27941
{{enp{{{{end procedure scngf{27942
{{ejc{{{{{27943
*      setvr -- set vrget,vrsto fields of vrblk
*      setvr sets the proper values in the vrget and vrsto
*      fields of a vrblk. it is called whenever trblks are
*      added or subtracted (trace,stoptr,input,output,detach)
*      (xr)                  pointer to vrblk
*      jsr  setvr            call to set fields
*      (xl,wa)               destroyed
*      note that setvr ignores the call if xr does not point
*      into the static region (i.e. is some other name base)
{setvr{prc{25,e{1,0{{entry point{27958
{{bhi{7,xr{3,state{6,setv1{exit if not natural variable{27959
*      here if we have a vrblk
{{mov{7,xl{7,xr{{copy vrblk pointer{27963
{{mov{13,vrget(xr){22,=b_vrl{{store normal get value{27964
{{beq{13,vrsto(xr){22,=b_vre{6,setv1{skip if protected variable{27965
{{mov{13,vrsto(xr){22,=b_vrs{{store normal store value{27966
{{mov{7,xl{13,vrval(xl){{point to next entry on chain{27967
{{bne{9,(xl){22,=b_trt{6,setv1{jump if end of trblk chain{27968
{{mov{13,vrget(xr){22,=b_vra{{store trapped routine address{27969
{{mov{13,vrsto(xr){22,=b_vrv{{set trapped routine address{27970
*      merge here to exit to caller
{setv1{exi{{{{return to setvr caller{27974
{{enp{{{{end procedure setvr{27975
{{ejc{{{{{27978
*      sorta -- sort array
*      routine to sort an array or table on same basis as in
*      sitbol. a table is converted to an array, leaving two
*      dimensional arrays and vectors as cases to be considered.
*      whole rows of arrays are permuted according to the
*      ordering of the keys they contain, and the stride
*      referred to, is the the length of a row. it is one
*      for a vector.
*      the sort used is heapsort, fundamentals of data structure
*      horowitz and sahni, pitman 1977, page 347.
*      it is an order n*log(n) algorithm. in order
*      to make it stable, comparands may not compare equal. this
*      is achieved by sorting a copy array (referred to as the
*      sort array) containing at its high address end, byte
*      offsets to the rows to be sorted held in the original
*      array (referred to as the key array). sortc, the
*      comparison routine, accesses the keys through these
*      offsets and in the case of equality, resolves it by
*      comparing the offsets themselves. the sort permutes the
*      offsets which are then used in a final operation to copy
*      the actual items into the new array in sorted order.
*      references to zeroth item are to notional item
*      preceding first actual item.
*      reverse sorting for rsort is done by having the less than
*      test for keys effectively be replaced by a
*      greater than test.
*      1(xs)                 first arg - array or table
*      0(xs)                 2nd arg - index or pdtype name
*      (wa)                  0 , non-zero for sort , rsort
*      jsr  sorta            call to sort array
*      ppm  loc              transfer loc if table is empty
*      (xr)                  sorted array
*      (xl,wa,wb,wc)         destroyed
{{ejc{{{{{28015
*      sorta (continued)
{sorta{prc{25,n{1,1{{entry point{28019
{{mov{3,srtsr{8,wa{{sort/rsort indicator{28020
{{mov{3,srtst{19,*num01{{default stride of 1{28021
{{zer{3,srtof{{{default zero offset to sort key{28022
{{mov{3,srtdf{21,=nulls{{clear datatype field name{28023
{{mov{3,r_sxr{10,(xs)+{{unstack argument 2{28024
{{mov{7,xr{10,(xs)+{{get first argument{28025
{{mnz{8,wa{{{use key/values of table entries{28026
{{jsr{6,gtarr{{{convert to array{28027
{{ppm{6,srt18{{{signal that table is empty{28028
{{ppm{6,srt16{{{error if non-convertable{28029
{{mov{11,-(xs){7,xr{{stack ptr to resulting key array{28030
{{mov{11,-(xs){7,xr{{another copy for copyb{28031
{{jsr{6,copyb{{{get copy array for sorting into{28032
{{ppm{{{{cant fail{28033
{{mov{11,-(xs){7,xr{{stack pointer to sort array{28034
{{mov{7,xr{3,r_sxr{{get second arg{28035
{{mov{7,xl{13,num01(xs){{get ptr to key array{28036
{{bne{9,(xl){22,=b_vct{6,srt02{jump if arblk{28037
{{beq{7,xr{21,=nulls{6,srt01{jump if null second arg{28038
{{jsr{6,gtnvr{{{get vrblk ptr for it{28039
{{err{1,257{26,erroneous 2nd arg in sort/rsort of vector{{{28040
{{mov{3,srtdf{7,xr{{store datatype field name vrblk{28041
*      compute n and offset to item a(0) in vector case
{srt01{mov{8,wc{19,*vclen{{offset to a(0){28045
{{mov{8,wb{19,*vcvls{{offset to first item{28046
{{mov{8,wa{13,vclen(xl){{get block length{28047
{{sub{8,wa{19,*vcsi_{{get no. of entries, n (in bytes){28048
{{brn{6,srt04{{{merge{28049
*      here for array
{srt02{ldi{13,ardim(xl){{{get possible dimension{28053
{{mfi{8,wa{{{convert to short integer{28054
{{wtb{8,wa{{{further convert to baus{28055
{{mov{8,wb{19,*arvls{{offset to first value if one{28056
{{mov{8,wc{19,*arpro{{offset before values if one dim.{28057
{{beq{13,arndm(xl){18,=num01{6,srt04{jump in fact if one dim.{28058
{{bne{13,arndm(xl){18,=num02{6,srt16{fail unless two dimens{28059
{{ldi{13,arlb2(xl){{{get lower bound 2 as default{28060
{{beq{7,xr{21,=nulls{6,srt03{jump if default second arg{28061
{{jsr{6,gtint{{{convert to integer{28062
{{ppm{6,srt17{{{fail{28063
{{ldi{13,icval(xr){{{get actual integer value{28064
{{ejc{{{{{28065
*      sorta (continued)
*      here with sort column index in ia in array case
{srt03{sbi{13,arlb2(xl){{{subtract low bound{28071
{{iov{6,srt17{{{fail if overflow{28072
{{ilt{6,srt17{{{fail if below low bound{28073
{{sbi{13,ardm2(xl){{{check against dimension{28074
{{ige{6,srt17{{{fail if too large{28075
{{adi{13,ardm2(xl){{{restore value{28076
{{mfi{8,wa{{{get as small integer{28077
{{wtb{8,wa{{{offset within row to key{28078
{{mov{3,srtof{8,wa{{keep offset{28079
{{ldi{13,ardm2(xl){{{second dimension is row length{28080
{{mfi{8,wa{{{convert to short integer{28081
{{mov{7,xr{8,wa{{copy row length{28082
{{wtb{8,wa{{{convert to bytes{28083
{{mov{3,srtst{8,wa{{store as stride{28084
{{ldi{13,ardim(xl){{{get number of rows{28085
{{mfi{8,wa{{{as a short integer{28086
{{wtb{8,wa{{{convert n to baus{28087
{{mov{8,wc{13,arlen(xl){{offset past array end{28088
{{sub{8,wc{8,wa{{adjust, giving space for n offsets{28089
{{dca{8,wc{{{point to a(0){28090
{{mov{8,wb{13,arofs(xl){{offset to word before first item{28091
{{ica{8,wb{{{offset to first item{28092
*      separate pre-processing for arrays and vectors done.
*      to simplify later key comparisons, removal of any trblk
*      trap blocks from entries in key array is effected.
*      (xl) = 1(xs) = pointer to key array
*      (xs) = pointer to sort array
*      wa = number of items, n (converted to bytes).
*      wb = offset to first item of arrays.
*      wc = offset to a(0)
{srt04{ble{8,wa{19,*num01{6,srt15{return if only a single item{28104
{{mov{3,srtsn{8,wa{{store number of items (in baus){28105
{{mov{3,srtso{8,wc{{store offset to a(0){28106
{{mov{8,wc{13,arlen(xl){{length of array or vec (=vclen){28107
{{add{8,wc{7,xl{{point past end of array or vector{28108
{{mov{3,srtsf{8,wb{{store offset to first row{28109
{{add{7,xl{8,wb{{point to first item in key array{28110
*      loop through array
{srt05{mov{7,xr{9,(xl){{get an entry{28114
*      hunt along trblk chain
{srt06{bne{9,(xr){22,=b_trt{6,srt07{jump out if not trblk{28118
{{mov{7,xr{13,trval(xr){{get value field{28119
{{brn{6,srt06{{{loop{28120
{{ejc{{{{{28121
*      sorta (continued)
*      xr is value from end of chain
{srt07{mov{10,(xl)+{7,xr{{store as array entry{28127
{{blt{7,xl{8,wc{6,srt05{loop if not done{28128
{{mov{7,xl{9,(xs){{get adrs of sort array{28129
{{mov{7,xr{3,srtsf{{initial offset to first key{28130
{{mov{8,wb{3,srtst{{get stride{28131
{{add{7,xl{3,srtso{{offset to a(0){28132
{{ica{7,xl{{{point to a(1){28133
{{mov{8,wc{3,srtsn{{get n{28134
{{btw{8,wc{{{convert from bytes{28135
{{mov{3,srtnr{8,wc{{store as row count{28136
{{lct{8,wc{8,wc{{loop counter{28137
*      store key offsets at top of sort array
{srt08{mov{10,(xl)+{7,xr{{store an offset{28141
{{add{7,xr{8,wb{{bump offset by stride{28142
{{bct{8,wc{6,srt08{{loop through rows{28143
*      perform the sort on offsets in sort array.
*      (srtsn)               number of items to sort, n (bytes)
*      (srtso)               offset to a(0)
{srt09{mov{8,wa{3,srtsn{{get n{28150
{{mov{8,wc{3,srtnr{{get number of rows{28151
{{rsh{8,wc{1,1{{i = n / 2 (wc=i, index into array){28152
{{wtb{8,wc{{{convert back to bytes{28153
*      loop to form initial heap
{srt10{jsr{6,sorth{{{sorth(i,n){28157
{{dca{8,wc{{{i = i - 1{28158
{{bnz{8,wc{6,srt10{{loop if i gt 0{28159
{{mov{8,wc{8,wa{{i = n{28160
*      sorting loop. at this point, a(1) is the largest
*      item, since algorithm initialises it as, and then maintains
*      it as, root of tree.
{srt11{dca{8,wc{{{i = i - 1 (n - 1 initially){28166
{{bze{8,wc{6,srt12{{jump if done{28167
{{mov{7,xr{9,(xs){{get sort array address{28168
{{add{7,xr{3,srtso{{point to a(0){28169
{{mov{7,xl{7,xr{{a(0) address{28170
{{add{7,xl{8,wc{{a(i) address{28171
{{mov{8,wb{13,num01(xl){{copy a(i+1){28172
{{mov{13,num01(xl){13,num01(xr){{move a(1) to a(i+1){28173
{{mov{13,num01(xr){8,wb{{complete exchange of a(1), a(i+1){28174
{{mov{8,wa{8,wc{{n = i for sorth{28175
{{mov{8,wc{19,*num01{{i = 1 for sorth{28176
{{jsr{6,sorth{{{sorth(1,n){28177
{{mov{8,wc{8,wa{{restore wc{28178
{{brn{6,srt11{{{loop{28179
{{ejc{{{{{28180
*      sorta (continued)
*      offsets have been permuted into required order by sort.
*      copy array elements over them.
{srt12{mov{7,xr{9,(xs){{base adrs of key array{28187
{{mov{8,wc{7,xr{{copy it{28188
{{add{8,wc{3,srtso{{offset of a(0){28189
{{add{7,xr{3,srtsf{{adrs of first row of sort array{28190
{{mov{8,wb{3,srtst{{get stride{28191
*      copying loop for successive items. sorted offsets are
*      held at end of sort array.
{srt13{ica{8,wc{{{adrs of next of sorted offsets{28196
{{mov{7,xl{8,wc{{copy it for access{28197
{{mov{7,xl{9,(xl){{get offset{28198
{{add{7,xl{13,num01(xs){{add key array base adrs{28199
{{mov{8,wa{8,wb{{get count of characters in row{28200
{{mvw{{{{copy a complete row{28201
{{dcv{3,srtnr{{{decrement row count{28202
{{bnz{3,srtnr{6,srt13{{repeat till all rows done{28203
*      return point
{srt15{mov{7,xr{10,(xs)+{{pop result array ptr{28207
{{ica{7,xs{{{pop key array ptr{28208
{{zer{3,r_sxl{{{clear junk{28209
{{zer{3,r_sxr{{{clear junk{28210
{{exi{{{{return{28211
*      error point
{srt16{erb{1,256{26,sort/rsort 1st arg not suitable array or table{{{28215
{srt17{erb{1,258{26,sort/rsort 2nd arg out of range or non-integer{{{28216
*      return point if input table is empty
{srt18{exi{1,1{{{return indication of null table{28220
{{enp{{{{end procudure sorta{28221
{{ejc{{{{{28222
*      sortc --  compare sort keys
*      compare two sort keys given their offsets. if
*      equal, compare key offsets to give stable sort.
*      note that if srtsr is non-zero (request for reverse
*      sort), the quoted returns are inverted.
*      for objects of differing datatypes, the entry point
*      identifications are compared.
*      (xl)                  base adrs for keys
*      (wa)                  offset to key 1 item
*      (wb)                  offset to key 2 item
*      (srtsr)               zero/non-zero for sort/rsort
*      (srtof)               offset within row to comparands
*      jsr  sortc            call to compare keys
*      ppm  loc              key1 less than key2
*                            normal return, key1 gt than key2
*      (xl,xr,wa,wb)         destroyed
{sortc{prc{25,e{1,1{{entry point{28243
{{mov{3,srts1{8,wa{{save offset 1{28244
{{mov{3,srts2{8,wb{{save offset 2{28245
{{mov{3,srtsc{8,wc{{save wc{28246
{{add{7,xl{3,srtof{{add offset to comparand field{28247
{{mov{7,xr{7,xl{{copy base + offset{28248
{{add{7,xl{8,wa{{add key1 offset{28249
{{add{7,xr{8,wb{{add key2 offset{28250
{{mov{7,xl{9,(xl){{get key1{28251
{{mov{7,xr{9,(xr){{get key2{28252
{{bne{3,srtdf{21,=nulls{6,src12{jump if datatype field name used{28253
{{ejc{{{{{28254
*      sortc (continued)
*      merge after dealing with field name. try for strings.
{src01{mov{8,wc{9,(xl){{get type code{28260
{{bne{8,wc{9,(xr){6,src02{skip if not same datatype{28261
{{beq{8,wc{22,=b_scl{6,src09{jump if both strings{28262
{{beq{8,wc{22,=b_icl{6,src14{jump if both integers{28263
*      datatypes different.  now try for numeric
{src02{mov{3,r_sxl{7,xl{{keep arg1{28271
{{mov{3,r_sxr{7,xr{{keep arg2{28272
{{beq{8,wc{22,=b_scl{6,src11{do not allow conversion to number{28275
{{beq{9,(xr){22,=b_scl{6,src11{if either arg is a string{28276
{src14{mov{11,-(xs){7,xl{{stack{28319
{{mov{11,-(xs){7,xr{{args{28320
{{jsr{6,acomp{{{compare objects{28321
{{ppm{6,src10{{{not numeric{28322
{{ppm{6,src10{{{not numeric{28323
{{ppm{6,src03{{{key1 less{28324
{{ppm{6,src08{{{keys equal{28325
{{ppm{6,src05{{{key1 greater{28326
*      return if key1 smaller (sort), greater (rsort)
{src03{bnz{3,srtsr{6,src06{{jump if rsort{28330
{src04{mov{8,wc{3,srtsc{{restore wc{28332
{{exi{1,1{{{return{28333
*      return if key1 greater (sort), smaller (rsort)
{src05{bnz{3,srtsr{6,src04{{jump if rsort{28337
{src06{mov{8,wc{3,srtsc{{restore wc{28339
{{exi{{{{return{28340
*      keys are of same datatype
{src07{blt{7,xl{7,xr{6,src03{item first created is less{28344
{{bgt{7,xl{7,xr{6,src05{addresses rise in order of creation{28345
*      drop through or merge for identical or equal objects
{src08{blt{3,srts1{3,srts2{6,src04{test offsets or key addrss instead{28349
{{brn{6,src06{{{offset 1 greater{28350
{{ejc{{{{{28351
*      sortc (continued)
*      strings
{src09{mov{11,-(xs){7,xl{{stack{28361
{{mov{11,-(xs){7,xr{{args{28362
{{jsr{6,lcomp{{{compare objects{28363
{{ppm{{{{cant{28364
{{ppm{{{{fail{28365
{{ppm{6,src03{{{key1 less{28366
{{ppm{6,src08{{{keys equal{28367
{{ppm{6,src05{{{key1 greater{28368
*      arithmetic comparison failed - recover args
{src10{mov{7,xl{3,r_sxl{{get arg1{28372
{{mov{7,xr{3,r_sxr{{get arg2{28373
{{mov{8,wc{9,(xl){{get type of key1{28374
{{beq{8,wc{9,(xr){6,src07{jump if keys of same type{28375
*      here to compare datatype ids
{src11{mov{7,xl{8,wc{{get block type word{28379
{{mov{7,xr{9,(xr){{get block type word{28380
{{lei{7,xl{{{entry point id for key1{28381
{{lei{7,xr{{{entry point id for key2{28382
{{bgt{7,xl{7,xr{6,src05{jump if key1 gt key2{28383
{{brn{6,src03{{{key1 lt key2{28384
*      datatype field name used
{src12{jsr{6,sortf{{{call routine to find field 1{28388
{{mov{11,-(xs){7,xl{{stack item pointer{28389
{{mov{7,xl{7,xr{{get key2{28390
{{jsr{6,sortf{{{find field 2{28391
{{mov{7,xr{7,xl{{place as key2{28392
{{mov{7,xl{10,(xs)+{{recover key1{28393
{{brn{6,src01{{{merge{28394
{{enp{{{{procedure sortc{28395
{{ejc{{{{{28396
*      sortf -- find field for sortc
*      routine used by sortc to obtain item corresponding
*      to a given field name, if this exists, in a programmer
*      defined object passed as argument.
*      if such a match occurs, record is kept of datatype
*      name, field name and offset to field in order to
*      short-circuit later searches on same type. note that
*      dfblks are stored in static and hence cannot be moved.
*      (srtdf)               vrblk pointer of field name
*      (xl)                  possible pdblk pointer
*      jsr  sortf            call to search for field name
*      (xl)                  item found or original pdblk ptr
*      (wc)                  destroyed
{sortf{prc{25,e{1,0{{entry point{28414
{{bne{9,(xl){22,=b_pdt{6,srtf3{return if not pdblk{28415
{{mov{11,-(xs){7,xr{{keep xr{28416
{{mov{7,xr{3,srtfd{{get possible former dfblk ptr{28417
{{bze{7,xr{6,srtf4{{jump if not{28418
{{bne{7,xr{13,pddfp(xl){6,srtf4{jump if not right datatype{28419
{{bne{3,srtdf{3,srtff{6,srtf4{jump if not right field name{28420
{{add{7,xl{3,srtfo{{add offset to required field{28421
*      here with xl pointing to found field
{srtf1{mov{7,xl{9,(xl){{get item from field{28425
*      return point
{srtf2{mov{7,xr{10,(xs)+{{restore xr{28429
{srtf3{exi{{{{return{28431
{{ejc{{{{{28432
*      sortf (continued)
*      conduct a search
{srtf4{mov{7,xr{7,xl{{copy original pointer{28438
{{mov{7,xr{13,pddfp(xr){{point to dfblk{28439
{{mov{3,srtfd{7,xr{{keep a copy{28440
{{mov{8,wc{13,fargs(xr){{get number of fields{28441
{{wtb{8,wc{{{convert to bytes{28442
{{add{7,xr{13,dflen(xr){{point past last field{28443
*      loop to find name in pdfblk
{srtf5{dca{8,wc{{{count down{28447
{{dca{7,xr{{{point in front{28448
{{beq{9,(xr){3,srtdf{6,srtf6{skip out if found{28449
{{bnz{8,wc{6,srtf5{{loop{28450
{{brn{6,srtf2{{{return - not found{28451
*      found
{srtf6{mov{3,srtff{9,(xr){{keep field name ptr{28455
{{add{8,wc{19,*pdfld{{add offset to first field{28456
{{mov{3,srtfo{8,wc{{store as field offset{28457
{{add{7,xl{8,wc{{point to field{28458
{{brn{6,srtf1{{{return{28459
{{enp{{{{procedure sortf{28460
{{ejc{{{{{28461
*      sorth -- heap routine for sorta
*      this routine constructs a heap from elements of array, a.
*      in this application, the elements are offsets to keys in
*      a key array.
*      (xs)                  pointer to sort array base
*      1(xs)                 pointer to key array base
*      (wa)                  max array index, n (in bytes)
*      (wc)                  offset j in a to root (in *1 to *n)
*      jsr  sorth            call sorth(j,n) to make heap
*      (xl,xr,wb)            destroyed
{sorth{prc{25,n{1,0{{entry point{28476
{{mov{3,srtsn{8,wa{{save n{28477
{{mov{3,srtwc{8,wc{{keep wc{28478
{{mov{7,xl{9,(xs){{sort array base adrs{28479
{{add{7,xl{3,srtso{{add offset to a(0){28480
{{add{7,xl{8,wc{{point to a(j){28481
{{mov{3,srtrt{9,(xl){{get offset to root{28482
{{add{8,wc{8,wc{{double j - cant exceed n{28483
*      loop to move down tree using doubled index j
{srh01{bgt{8,wc{3,srtsn{6,srh03{done if j gt n{28487
{{beq{8,wc{3,srtsn{6,srh02{skip if j equals n{28488
{{mov{7,xr{9,(xs){{sort array base adrs{28489
{{mov{7,xl{13,num01(xs){{key array base adrs{28490
{{add{7,xr{3,srtso{{point to a(0){28491
{{add{7,xr{8,wc{{adrs of a(j){28492
{{mov{8,wa{13,num01(xr){{get a(j+1){28493
{{mov{8,wb{9,(xr){{get a(j){28494
*      compare sons. (wa) right son, (wb) left son
{{jsr{6,sortc{{{compare keys - lt(a(j+1),a(j)){28498
{{ppm{6,srh02{{{a(j+1) lt a(j){28499
{{ica{8,wc{{{point to greater son, a(j+1){28500
{{ejc{{{{{28501
*      sorth (continued)
*      compare root with greater son
{srh02{mov{7,xl{13,num01(xs){{key array base adrs{28507
{{mov{7,xr{9,(xs){{get sort array address{28508
{{add{7,xr{3,srtso{{adrs of a(0){28509
{{mov{8,wb{7,xr{{copy this adrs{28510
{{add{7,xr{8,wc{{adrs of greater son, a(j){28511
{{mov{8,wa{9,(xr){{get a(j){28512
{{mov{7,xr{8,wb{{point back to a(0){28513
{{mov{8,wb{3,srtrt{{get root{28514
{{jsr{6,sortc{{{compare them - lt(a(j),root){28515
{{ppm{6,srh03{{{father exceeds sons - done{28516
{{mov{7,xr{9,(xs){{get sort array adrs{28517
{{add{7,xr{3,srtso{{point to a(0){28518
{{mov{7,xl{7,xr{{copy it{28519
{{mov{8,wa{8,wc{{copy j{28520
{{btw{8,wc{{{convert to words{28521
{{rsh{8,wc{1,1{{get j/2{28522
{{wtb{8,wc{{{convert back to bytes{28523
{{add{7,xl{8,wa{{point to a(j){28524
{{add{7,xr{8,wc{{adrs of a(j/2){28525
{{mov{9,(xr){9,(xl){{a(j/2) = a(j){28526
{{mov{8,wc{8,wa{{recover j{28527
{{aov{8,wc{8,wc{6,srh03{j = j*2. done if too big{28528
{{brn{6,srh01{{{loop{28529
*      finish by copying root offset back into array
{srh03{btw{8,wc{{{convert to words{28533
{{rsh{8,wc{1,1{{j = j/2{28534
{{wtb{8,wc{{{convert back to bytes{28535
{{mov{7,xr{9,(xs){{sort array adrs{28536
{{add{7,xr{3,srtso{{adrs of a(0){28537
{{add{7,xr{8,wc{{adrs of a(j/2){28538
{{mov{9,(xr){3,srtrt{{a(j/2) = root{28539
{{mov{8,wa{3,srtsn{{restore wa{28540
{{mov{8,wc{3,srtwc{{restore wc{28541
{{exi{{{{return{28542
{{enp{{{{end procedure sorth{28543
{{ejc{{{{{28545
*      trace -- set/reset a trace association
*      this procedure is shared by trace and stoptr to
*      either initiate or stop a trace respectively.
*      (xl)                  trblk ptr (trace) or zero (stoptr)
*      1(xs)                 first argument (name)
*      0(xs)                 second argument (trace type)
*      jsr  trace            call to set/reset trace
*      ppm  loc              transfer loc if 1st arg is bad name
*      ppm  loc              transfer loc if 2nd arg is bad type
*      (xs)                  popped
*      (xl,xr,wa,wb,wc,ia)   destroyed
{trace{prc{25,n{1,2{{entry point{28561
{{jsr{6,gtstg{{{get trace type string{28562
{{ppm{6,trc15{{{jump if not string{28563
{{plc{7,xr{{{else point to string{28564
{{lch{8,wa{9,(xr){{load first character{28565
{{mov{7,xr{9,(xs){{load name argument{28569
{{mov{9,(xs){7,xl{{stack trblk ptr or zero{28570
{{mov{8,wc{18,=trtac{{set trtyp for access trace{28571
{{beq{8,wa{18,=ch_la{6,trc10{jump if a (access){28572
{{mov{8,wc{18,=trtvl{{set trtyp for value trace{28573
{{beq{8,wa{18,=ch_lv{6,trc10{jump if v (value){28574
{{beq{8,wa{18,=ch_bl{6,trc10{jump if blank (value){28575
*      here for l,k,f,c,r
{{beq{8,wa{18,=ch_lf{6,trc01{jump if f (function){28579
{{beq{8,wa{18,=ch_lr{6,trc01{jump if r (return){28580
{{beq{8,wa{18,=ch_ll{6,trc03{jump if l (label){28581
{{beq{8,wa{18,=ch_lk{6,trc06{jump if k (keyword){28582
{{bne{8,wa{18,=ch_lc{6,trc15{else error if not c (call){28583
*      here for f,c,r
{trc01{jsr{6,gtnvr{{{point to vrblk for name{28587
{{ppm{6,trc16{{{jump if bad name{28588
{{ica{7,xs{{{pop stack{28589
{{mov{7,xr{13,vrfnc(xr){{point to function block{28590
{{bne{9,(xr){22,=b_pfc{6,trc17{error if not program function{28591
{{beq{8,wa{18,=ch_lr{6,trc02{jump if r (return){28592
{{ejc{{{{{28593
*      trace (continued)
*      here for f,c to set/reset call trace
{{mov{13,pfctr(xr){7,xl{{set/reset call trace{28599
{{beq{8,wa{18,=ch_lc{6,exnul{exit with null if c (call){28600
*      here for f,r to set/reset return trace
{trc02{mov{13,pfrtr(xr){7,xl{{set/reset return trace{28604
{{exi{{{{return{28605
*      here for l to set/reset label trace
{trc03{jsr{6,gtnvr{{{point to vrblk{28609
{{ppm{6,trc16{{{jump if bad name{28610
{{mov{7,xl{13,vrlbl(xr){{load label pointer{28611
{{bne{9,(xl){22,=b_trt{6,trc04{jump if no old trace{28612
{{mov{7,xl{13,trlbl(xl){{else delete old trace association{28613
*      here with old label trace association deleted
{trc04{beq{7,xl{21,=stndl{6,trc16{error if undefined label{28617
{{mov{8,wb{10,(xs)+{{get trblk ptr again{28618
{{bze{8,wb{6,trc05{{jump if stoptr case{28619
{{mov{13,vrlbl(xr){8,wb{{else set new trblk pointer{28620
{{mov{13,vrtra(xr){22,=b_vrt{{set label trace routine address{28621
{{mov{7,xr{8,wb{{copy trblk pointer{28622
{{mov{13,trlbl(xr){7,xl{{store real label in trblk{28623
{{exi{{{{return{28624
*      here for stoptr case for label
{trc05{mov{13,vrlbl(xr){7,xl{{store label ptr back in vrblk{28628
{{mov{13,vrtra(xr){22,=b_vrg{{store normal transfer address{28629
{{exi{{{{return{28630
{{ejc{{{{{28631
*      trace (continued)
*      here for k (keyword)
{trc06{jsr{6,gtnvr{{{point to vrblk{28637
{{ppm{6,trc16{{{error if not natural var{28638
{{bnz{13,vrlen(xr){6,trc16{{error if not system var{28639
{{ica{7,xs{{{pop stack{28640
{{bze{7,xl{6,trc07{{jump if stoptr case{28641
{{mov{13,trkvr(xl){7,xr{{store vrblk ptr in trblk for ktrex{28642
*      merge here with trblk set up in wb (or zero)
{trc07{mov{7,xr{13,vrsvp(xr){{point to svblk{28646
{{beq{7,xr{21,=v_ert{6,trc08{jump if errtype{28647
{{beq{7,xr{21,=v_stc{6,trc09{jump if stcount{28648
{{bne{7,xr{21,=v_fnc{6,trc17{else error if not fnclevel{28649
*      fnclevel
{{mov{3,r_fnc{7,xl{{set/reset fnclevel trace{28653
{{exi{{{{return{28654
*      errtype
{trc08{mov{3,r_ert{7,xl{{set/reset errtype trace{28658
{{exi{{{{return{28659
*      stcount
{trc09{mov{3,r_stc{7,xl{{set/reset stcount trace{28663
{{jsr{6,stgcc{{{update countdown counters{28664
{{exi{{{{return{28665
{{ejc{{{{{28666
*      trace (continued)
*      a,v merge here with trtyp value in wc
{trc10{jsr{6,gtvar{{{locate variable{28672
{{ppm{6,trc16{{{error if not appropriate name{28673
{{mov{8,wb{10,(xs)+{{get new trblk ptr again{28674
{{add{8,wa{7,xl{{point to variable location{28675
{{mov{7,xr{8,wa{{copy variable pointer{28676
*      loop to search trblk chain
{trc11{mov{7,xl{9,(xr){{point to next entry{28680
{{bne{9,(xl){22,=b_trt{6,trc13{jump if not trblk{28681
{{blt{8,wc{13,trtyp(xl){6,trc13{jump if too far out on chain{28682
{{beq{8,wc{13,trtyp(xl){6,trc12{jump if this matches our type{28683
{{add{7,xl{19,*trnxt{{else point to link field{28684
{{mov{7,xr{7,xl{{copy pointer{28685
{{brn{6,trc11{{{and loop back{28686
*      here to delete an old trblk of the type we were given
{trc12{mov{7,xl{13,trnxt(xl){{get ptr to next block or value{28690
{{mov{9,(xr){7,xl{{store to delete this trblk{28691
*      here after deleting any old association of this type
{trc13{bze{8,wb{6,trc14{{jump if stoptr case{28695
{{mov{9,(xr){8,wb{{else link new trblk in{28696
{{mov{7,xr{8,wb{{copy trblk pointer{28697
{{mov{13,trnxt(xr){7,xl{{store forward pointer{28698
{{mov{13,trtyp(xr){8,wc{{store appropriate trap type code{28699
*      here to make sure vrget,vrsto are set properly
{trc14{mov{7,xr{8,wa{{recall possible vrblk pointer{28703
{{sub{7,xr{19,*vrval{{point back to vrblk{28704
{{jsr{6,setvr{{{set fields if vrblk{28705
{{exi{{{{return{28706
*      here for bad trace type
{trc15{exi{1,2{{{take bad trace type error exit{28710
*      pop stack before failing
{trc16{ica{7,xs{{{pop stack{28714
*      here for bad name argument
{trc17{exi{1,1{{{take bad name error exit{28718
{{enp{{{{end procedure trace{28719
{{ejc{{{{{28720
*      trbld -- build trblk
*      trblk is used by the input, output and trace functions
*      to construct a trblk (trap block)
*      (xr)                  trtag or trter
*      (xl)                  trfnc or trfpt
*      (wb)                  trtyp
*      jsr  trbld            call to build trblk
*      (xr)                  pointer to trblk
*      (wa)                  destroyed
{trbld{prc{25,e{1,0{{entry point{28734
{{mov{11,-(xs){7,xr{{stack trtag (or trfnm){28735
{{mov{8,wa{19,*trsi_{{set size of trblk{28736
{{jsr{6,alloc{{{allocate trblk{28737
{{mov{9,(xr){22,=b_trt{{store first word{28738
{{mov{13,trfnc(xr){7,xl{{store trfnc (or trfpt){28739
{{mov{13,trtag(xr){10,(xs)+{{store trtag (or trfnm){28740
{{mov{13,trtyp(xr){8,wb{{store type{28741
{{mov{13,trval(xr){21,=nulls{{for now, a null value{28742
{{exi{{{{return to caller{28743
{{enp{{{{end procedure trbld{28744
{{ejc{{{{{28745
*      trimr -- trim trailing blanks
*      trimr is passed a pointer to an scblk which must be the
*      last block in dynamic storage. trailing blanks are
*      trimmed off and the dynamic storage pointer reset to
*      the end of the (possibly) shortened block.
*      (wb)                  non-zero to trim trailing blanks
*      (xr)                  pointer to string to trim
*      jsr  trimr            call to trim string
*      (xr)                  pointer to trimmed string
*      (xl,wa,wb,wc)         destroyed
*      the call with wb zero still performs the end zero pad
*      and dnamp readjustment. it is used from acess if kvtrm=0.
{trimr{prc{25,e{1,0{{entry point{28763
{{mov{7,xl{7,xr{{copy string pointer{28764
{{mov{8,wa{13,sclen(xr){{load string length{28765
{{bze{8,wa{6,trim2{{jump if null input{28766
{{plc{7,xl{8,wa{{else point past last character{28767
{{bze{8,wb{6,trim3{{jump if no trim{28768
{{mov{8,wc{18,=ch_bl{{load blank character{28769
*      loop through characters from right to left
{trim0{lch{8,wb{11,-(xl){{load next character{28773
{{beq{8,wb{18,=ch_ht{6,trim1{jump if horizontal tab{28775
{{bne{8,wb{8,wc{6,trim3{jump if non-blank found{28777
{trim1{dcv{8,wa{{{else decrement character count{28778
{{bnz{8,wa{6,trim0{{loop back if more to check{28779
*      here if result is null (null or all-blank input)
{trim2{mov{3,dnamp{7,xr{{wipe out input string block{28783
{{mov{7,xr{21,=nulls{{load null result{28784
{{brn{6,trim5{{{merge to exit{28785
{{ejc{{{{{28786
*      trimr (continued)
*      here with non-blank found (merge for no trim)
{trim3{mov{13,sclen(xr){8,wa{{set new length{28792
{{mov{7,xl{7,xr{{copy string pointer{28793
{{psc{7,xl{8,wa{{ready for storing blanks{28794
{{ctb{8,wa{2,schar{{get length of block in bytes{28795
{{add{8,wa{7,xr{{point past new block{28796
{{mov{3,dnamp{8,wa{{set new top of storage pointer{28797
{{lct{8,wa{18,=cfp_c{{get count of chars in word{28798
{{zer{8,wc{{{set zero char{28799
*      loop to zero pad last word of characters
{trim4{sch{8,wc{10,(xl)+{{store zero character{28803
{{bct{8,wa{6,trim4{{loop back till all stored{28804
{{csc{7,xl{{{complete store characters{28805
*      common exit point
{trim5{zer{7,xl{{{clear garbage xl pointer{28809
{{exi{{{{return to caller{28810
{{enp{{{{end procedure trimr{28811
{{ejc{{{{{28812
*      trxeq -- execute function type trace
*      trxeq is used to execute a trace when a fourth argument
*      has been supplied. trace has already been decremented.
*      (xr)                  pointer to trblk
*      (xl,wa)               name base,offset for variable
*      jsr  trxeq            call to execute trace
*      (wb,wc,ra)            destroyed
*      the following stack entries are made before passing
*      control to the trace function using the cfunc routine.
*                            trxeq return point word(s)
*                            saved value of trace keyword
*                            trblk pointer
*                            name base
*                            name offset
*                            saved value of r_cod
*                            saved code ptr (-r_cod)
*                            saved value of flptr
*      flptr --------------- zero (dummy fail offset)
*                            nmblk for variable name
*      xs ------------------ trace tag
*      r_cod and the code ptr are set to dummy values which
*      cause control to return to the trxeq procedure on success
*      or failure (trxeq ignores a failure condition).
{trxeq{prc{25,r{1,0{{entry point (recursive){28843
{{mov{8,wc{3,r_cod{{load code block pointer{28844
{{scp{8,wb{{{get current code pointer{28845
{{sub{8,wb{8,wc{{make code pointer into offset{28846
{{mov{11,-(xs){3,kvtra{{stack trace keyword value{28847
{{mov{11,-(xs){7,xr{{stack trblk pointer{28848
{{mov{11,-(xs){7,xl{{stack name base{28849
{{mov{11,-(xs){8,wa{{stack name offset{28850
{{mov{11,-(xs){8,wc{{stack code block pointer{28851
{{mov{11,-(xs){8,wb{{stack code pointer offset{28852
{{mov{11,-(xs){3,flptr{{stack old failure pointer{28853
{{zer{11,-(xs){{{set dummy fail offset{28854
{{mov{3,flptr{7,xs{{set new failure pointer{28855
{{zer{3,kvtra{{{reset trace keyword to zero{28856
{{mov{8,wc{21,=trxdc{{load new (dummy) code blk pointer{28857
{{mov{3,r_cod{8,wc{{set as code block pointer{28858
{{lcp{8,wc{{{and new code pointer{28859
{{ejc{{{{{28860
*      trxeq (continued)
*      now prepare arguments for function
{{mov{8,wb{8,wa{{save name offset{28866
{{mov{8,wa{19,*nmsi_{{load nmblk size{28867
{{jsr{6,alloc{{{allocate space for nmblk{28868
{{mov{9,(xr){22,=b_nml{{set type word{28869
{{mov{13,nmbas(xr){7,xl{{store name base{28870
{{mov{13,nmofs(xr){8,wb{{store name offset{28871
{{mov{7,xl{12,6(xs){{reload pointer to trblk{28872
{{mov{11,-(xs){7,xr{{stack nmblk pointer (1st argument){28873
{{mov{11,-(xs){13,trtag(xl){{stack trace tag (2nd argument){28874
{{mov{7,xl{13,trfnc(xl){{load trace vrblk pointer{28875
{{mov{7,xl{13,vrfnc(xl){{load trace function pointer{28876
{{beq{7,xl{21,=stndf{6,trxq2{jump if not a defined function{28877
{{mov{8,wa{18,=num02{{set number of arguments to two{28878
{{brn{6,cfunc{{{jump to call function{28879
*      see o_txr for details of return to this point
{trxq1{mov{7,xs{3,flptr{{point back to our stack entries{28883
{{ica{7,xs{{{pop off garbage fail offset{28884
{{mov{3,flptr{10,(xs)+{{restore old failure pointer{28885
{{mov{8,wb{10,(xs)+{{reload code offset{28886
{{mov{8,wc{10,(xs)+{{load old code base pointer{28887
{{mov{7,xr{8,wc{{copy cdblk pointer{28888
{{mov{3,kvstn{13,cdstm(xr){{restore stmnt no{28889
{{mov{8,wa{10,(xs)+{{reload name offset{28890
{{mov{7,xl{10,(xs)+{{reload name base{28891
{{mov{7,xr{10,(xs)+{{reload trblk pointer{28892
{{mov{3,kvtra{10,(xs)+{{restore trace keyword value{28893
{{add{8,wb{8,wc{{recompute absolute code pointer{28894
{{lcp{8,wb{{{restore code pointer{28895
{{mov{3,r_cod{8,wc{{and code block pointer{28896
{{exi{{{{return to trxeq caller{28897
*      here if the target function is not defined
{trxq2{erb{1,197{26,trace fourth arg is not function name or null{{{28901
{{enp{{{{end procedure trxeq{28903
{{ejc{{{{{28904
*      xscan -- execution function argument scan
*      xscan scans out one token in a prototype argument in
*      array,clear,data,define,load function calls. xscan
*      calls must be preceded by a call to the initialization
*      procedure xscni. the following variables are used.
*      r_xsc                 pointer to scblk for function arg
*      xsofs                 offset (num chars scanned so far)
*      (wa)                  non-zero to skip and trim blanks
*      (wc)                  delimiter one (ch_xx)
*      (xl)                  delimiter two (ch_xx)
*      jsr  xscan            call to scan next item
*      (xr)                  pointer to scblk for token scanned
*      (wa)                  completion code (see below)
*      (wc,xl)               destroyed
*      the scan starts from the current position and continues
*      until one of the following three conditions occurs.
*      1)   delimiter one is encountered  (wa set to 1)
*      2)   delimiter two encountered  (wa set to 2)
*      3)   end of string encountered  (wa set to 0)
*      the result is a string containing all characters scanned
*      up to but not including any delimiter character.
*      the pointer is left pointing past the delimiter.
*      if only one delimiter is to be detected, delimiter one
*      and delimiter two should be set to the same value.
*      in the case where the end of string is encountered, the
*      string includes all the characters to the end of the
*      string. no further calls can be made to xscan until
*      xscni is called to initialize a new argument scan
{{ejc{{{{{28944
*      xscan (continued)
{xscan{prc{25,e{1,0{{entry point{28948
{{mov{3,xscwb{8,wb{{preserve wb{28949
{{mov{11,-(xs){8,wa{{record blank skip flag{28950
{{mov{11,-(xs){8,wa{{and second copy{28951
{{mov{7,xr{3,r_xsc{{point to argument string{28952
{{mov{8,wa{13,sclen(xr){{load string length{28953
{{mov{8,wb{3,xsofs{{load current offset{28954
{{sub{8,wa{8,wb{{get number of remaining characters{28955
{{bze{8,wa{6,xscn3{{jump if no characters left{28956
{{plc{7,xr{8,wb{{point to current character{28957
*      loop to search for delimiter
{xscn1{lch{8,wb{10,(xr)+{{load next character{28961
{{beq{8,wb{8,wc{6,xscn4{jump if delimiter one found{28962
{{beq{8,wb{7,xl{6,xscn5{jump if delimiter two found{28963
{{bze{9,(xs){6,xscn2{{jump if not skipping blanks{28964
{{icv{3,xsofs{{{assume blank and delete it{28965
{{beq{8,wb{18,=ch_ht{6,xscn2{jump if horizontal tab{28967
{{beq{8,wb{18,=ch_bl{6,xscn2{jump if blank{28972
{{dcv{3,xsofs{{{undelete non-blank character{28973
{{zer{9,(xs){{{and discontinue blank checking{28974
*      here after performing any leading blank trimming.
{xscn2{dcv{8,wa{{{decrement count of chars left{28978
{{bnz{8,wa{6,xscn1{{loop back if more chars to go{28979
*      here for runout
{xscn3{mov{7,xl{3,r_xsc{{point to string block{28983
{{mov{8,wa{13,sclen(xl){{get string length{28984
{{mov{8,wb{3,xsofs{{load offset{28985
{{sub{8,wa{8,wb{{get substring length{28986
{{zer{3,r_xsc{{{clear string ptr for collector{28987
{{zer{3,xscrt{{{set zero (runout) return code{28988
{{brn{6,xscn7{{{jump to exit{28989
{{ejc{{{{{28990
*      xscan (continued)
*      here if delimiter one found
{xscn4{mov{3,xscrt{18,=num01{{set return code{28996
{{brn{6,xscn6{{{jump to merge{28997
*      here if delimiter two found
{xscn5{mov{3,xscrt{18,=num02{{set return code{29001
*      merge here after detecting a delimiter
{xscn6{mov{7,xl{3,r_xsc{{reload pointer to string{29005
{{mov{8,wc{13,sclen(xl){{get original length of string{29006
{{sub{8,wc{8,wa{{minus chars left = chars scanned{29007
{{mov{8,wa{8,wc{{move to reg for sbstr{29008
{{mov{8,wb{3,xsofs{{set offset{29009
{{sub{8,wa{8,wb{{compute length for sbstr{29010
{{icv{8,wc{{{adjust new cursor past delimiter{29011
{{mov{3,xsofs{8,wc{{store new offset{29012
*      common exit point
{xscn7{zer{7,xr{{{clear garbage character ptr in xr{29016
{{jsr{6,sbstr{{{build sub-string{29017
{{ica{7,xs{{{remove copy of blank flag{29018
{{mov{8,wb{10,(xs)+{{original blank skip/trim flag{29019
{{bze{13,sclen(xr){6,xscn8{{cannot trim the null string{29020
{{jsr{6,trimr{{{trim trailing blanks if requested{29021
*      final exit point
{xscn8{mov{8,wa{3,xscrt{{load return code{29025
{{mov{8,wb{3,xscwb{{restore wb{29026
{{exi{{{{return to xscan caller{29027
{{enp{{{{end procedure xscan{29028
{{ejc{{{{{29029
*      xscni -- execution function argument scan
*      xscni initializes the scan used for prototype arguments
*      in the clear, define, load, data, array functions. see
*      xscan for the procedure which is used after this call.
*      -(xs)                 argument to be scanned (on stack)
*      jsr  xscni            call to scan argument
*      ppm  loc              transfer loc if arg is not string
*      ppm  loc              transfer loc if argument is null
*      (xs)                  popped
*      (xr,r_xsc)            argument (scblk ptr)
*      (wa)                  argument length
*      (ia,ra)               destroyed
{xscni{prc{25,n{1,2{{entry point{29046
{{jsr{6,gtstg{{{fetch argument as string{29047
{{ppm{6,xsci1{{{jump if not convertible{29048
{{mov{3,r_xsc{7,xr{{else store scblk ptr for xscan{29049
{{zer{3,xsofs{{{set offset to zero{29050
{{bze{8,wa{6,xsci2{{jump if null string{29051
{{exi{{{{return to xscni caller{29052
*      here if argument is not a string
{xsci1{exi{1,1{{{take not-string error exit{29056
*      here for null string
{xsci2{exi{1,2{{{take null-string error exit{29060
{{enp{{{{end procedure xscni{29061
{{ttl{27,s p i t b o l -- stack overflow section{{{{29062
*      control comes here if the main stack overflows
{{sec{{{{start of stack overflow section{29066
{{add{3,errft{18,=num04{{force conclusive fatal error{29068
{{mov{7,xs{3,flptr{{pop stack to avoid more fails{29069
{{bnz{3,gbcfl{6,stak1{{jump if garbage collecting{29070
{{erb{1,246{26,stack overflow{{{29071
*      no chance of recovery in mid garbage collection
{stak1{mov{7,xr{21,=endso{{point to message{29075
{{zer{3,kvdmp{{{memory is undumpable{29076
{{brn{6,stopr{{{give up{29077
{{ttl{27,s p i t b o l -- error section{{{{29078
*      this section of code is entered whenever a procedure
*      return via an err parameter or an erb opcode is obeyed.
*      (wa)                  is the error code
*      the global variable stage indicates the point at which
*      the error occured as follows.
*      stage=stgic           error during initial compile
*      stage=stgxc           error during compile at execute
*                            time (code, convert function calls)
*      stage=stgev           error during compilation of
*                            expression at execution time
*                            (eval, convert function call).
*      stage=stgxt           error at execute time. compiler
*                            not active.
*      stage=stgce           error during initial compile after
*                            scanning out the end line.
*      stage=stgxe           error during compile at execute
*                            time after scanning end line.
*      stage=stgee           error during expression evaluation
{{sec{{{{start of error section{29108
{error{beq{3,r_cim{20,=cmlab{6,cmple{jump if error in scanning label{29110
{{mov{3,kvert{8,wa{{save error code{29111
{{zer{3,scnrs{{{reset rescan switch for scane{29112
{{zer{3,scngo{{{reset goto switch for scane{29113
{{mov{3,polcs{18,=num01{{reset poll count{29115
{{mov{3,polct{18,=num01{{reset poll count{29116
{{mov{7,xr{3,stage{{load current stage{29118
{{bsw{7,xr{2,stgno{{jump to appropriate error circuit{29119
{{iff{2,stgic{6,err01{{initial compile{29127
{{iff{2,stgxc{6,err04{{execute time compile{29127
{{iff{2,stgev{6,err04{{eval compiling expr.{29127
{{iff{2,stgxt{6,err05{{execute time{29127
{{iff{2,stgce{6,err01{{compile - after end{29127
{{iff{2,stgxe{6,err04{{xeq compile-past end{29127
{{iff{2,stgee{6,err04{{eval evaluating expr{29127
{{esw{{{{end switch on error type{29127
{{ejc{{{{{29128
*      error during initial compile
*      the error message is printed as part of the compiler
*      output. this printout includes the offending line (if not
*      printed already) and an error flag under the appropriate
*      column as indicated by scnse unless scnse is set to zero.
*      after printing the message, the generated code is
*      modified to an error call and control is returned to
*      the cmpil procedure after resetting the stack pointer.
*      if the error occurs after the end line, control returns
*      in a slightly different manner to ensure proper cleanup.
{err01{mov{7,xs{3,cmpxs{{reset stack pointer{29144
{{ssl{3,cmpss{{{restore s-r stack ptr for cmpil{29145
{{bnz{3,errsp{6,err03{{jump if error suppress flag set{29146
{{mov{8,wc{3,cmpsn{{current statement{29149
{{jsr{6,filnm{{{obtain file name for this statement{29150
{{mov{8,wb{3,scnse{{column number{29152
{{mov{8,wc{3,rdcln{{line number{29153
{{mov{7,xr{3,stage{{{29154
{{jsr{6,sysea{{{advise system of error{29155
{{ppm{6,erra3{{{if system does not want print{29156
{{mov{11,-(xs){7,xr{{save any provided print message{29157
{{mov{3,erlst{3,erich{{set flag for listr{29159
{{jsr{6,listr{{{list line{29160
{{jsr{6,prtis{{{terminate listing{29161
{{zer{3,erlst{{{clear listr flag{29162
{{mov{8,wa{3,scnse{{load scan element offset{29163
{{bze{8,wa{6,err02{{skip if not set{29164
{{lct{8,wb{8,wa{{loop counter{29166
{{icv{8,wa{{{increase for ch_ex{29167
{{mov{7,xl{3,r_cim{{point to bad statement{29168
{{jsr{6,alocs{{{string block for error flag{29169
{{mov{8,wa{7,xr{{remember string ptr{29170
{{psc{7,xr{{{ready for character storing{29171
{{plc{7,xl{{{ready to get chars{29172
*      loop to replace all chars but tabs by blanks
{erra1{lch{8,wc{10,(xl)+{{get next char{29176
{{beq{8,wc{18,=ch_ht{6,erra2{skip if tab{29177
{{mov{8,wc{18,=ch_bl{{get a blank{29178
{{ejc{{{{{29179
*      merge to store blank or tab in error line
{erra2{sch{8,wc{10,(xr)+{{store char{29183
{{bct{8,wb{6,erra1{{loop{29184
{{mov{7,xl{18,=ch_ex{{exclamation mark{29185
{{sch{7,xl{9,(xr){{store at end of error line{29186
{{csc{7,xr{{{end of sch loop{29187
{{mov{3,profs{18,=stnpd{{allow for statement number{29188
{{mov{7,xr{8,wa{{point to error line{29189
{{jsr{6,prtst{{{print error line{29190
*      here after placing error flag as required
{err02{jsr{6,prtis{{{print blank line{29204
{{mov{7,xr{10,(xs)+{{restore any sysea message{29206
{{bze{7,xr{6,erra0{{did sysea provide message to print{29207
{{jsr{6,prtst{{{print sysea message{29208
{erra0{jsr{6,ermsg{{{generate flag and error message{29210
{{add{3,lstlc{18,=num03{{bump page ctr for blank, error, blk{29211
{erra3{zer{7,xr{{{in case of fatal error{29212
{{bhi{3,errft{18,=num03{6,stopr{pack up if several fatals{29213
*      count error, inhibit execution if required
{{icv{3,cmerc{{{bump error count{29217
{{add{3,noxeq{3,cswer{{inhibit xeq if -noerrors{29218
{{bne{3,stage{18,=stgic{6,cmp10{special return if after end line{29219
{{ejc{{{{{29220
*      loop to scan to end of statement
{err03{mov{7,xr{3,r_cim{{point to start of image{29224
{{plc{7,xr{{{point to first char{29225
{{lch{7,xr{9,(xr){{get first char{29226
{{beq{7,xr{18,=ch_mn{6,cmpce{jump if error in control card{29227
{{zer{3,scnrs{{{clear rescan flag{29228
{{mnz{3,errsp{{{set error suppress flag{29229
{{jsr{6,scane{{{scan next element{29230
{{bne{7,xl{18,=t_smc{6,err03{loop back if not statement end{29231
{{zer{3,errsp{{{clear error suppress flag{29232
*      generate error call in code and return to cmpil
{{mov{3,cwcof{19,*cdcod{{reset offset in ccblk{29236
{{mov{8,wa{21,=ocer_{{load compile error call{29237
{{jsr{6,cdwrd{{{generate it{29238
{{mov{13,cmsoc(xs){3,cwcof{{set success fill in offset{29239
{{mnz{13,cmffc(xs){{{set failure fill in flag{29240
{{jsr{6,cdwrd{{{generate succ. fill in word{29241
{{brn{6,cmpse{{{merge to generate error as cdfal{29242
*      error during execute time compile or expression evaluatio
*      execute time compilation is initiated through gtcod or
*      gtexp which are called by compile, code or eval.
*      before causing statement failure through exfal it is
*      helpful to set keyword errtext and for generality
*      these errors may be handled by the setexit mechanism.
{err04{bge{3,errft{18,=num03{6,labo1{abort if too many fatal errors{29252
{{beq{3,kvert{18,=nm320{6,err06{treat user interrupt specially{29254
{{zer{3,r_ccb{{{forget garbage code block{29256
{{mov{3,cwcof{19,*cccod{{set initial offset (mbe catspaw){29257
{{ssl{3,iniss{{{restore main prog s-r stack ptr{29258
{{jsr{6,ertex{{{get fail message text{29259
{{dca{7,xs{{{ensure stack ok on loop start{29260
*      pop stack until find flptr for most deeply nested prog.
*      defined function call or call of eval / code.
{erra4{ica{7,xs{{{pop stack{29265
{{beq{7,xs{3,flprt{6,errc4{jump if prog defined fn call found{29266
{{bne{7,xs{3,gtcef{6,erra4{loop if not eval or code call yet{29267
{{mov{3,stage{18,=stgxt{{re-set stage for execute{29268
{{mov{3,r_cod{3,r_gtc{{recover code ptr{29269
{{mov{3,flptr{7,xs{{restore fail pointer{29270
{{zer{3,r_cim{{{forget possible image{29271
{{zer{3,cnind{{{forget possible include{29273
*      test errlimit
{errb4{bnz{3,kverl{6,err07{{jump if errlimit non-zero{29278
{{brn{6,exfal{{{fail{29279
*      return from prog. defined function is outstanding
{errc4{mov{7,xs{3,flptr{{restore stack from flptr{29283
{{brn{6,errb4{{{merge{29284
{{ejc{{{{{29285
*      error at execute time.
*      the action taken on an error is as follows.
*      if errlimit keyword is zero, an abort is signalled,
*      see coding for system label abort at l_abo.
*      otherwise, errlimit is decremented and an errtype trace
*      generated if required. control returns either via a jump
*      to continue (to take the failure exit) or a specified
*      setexit trap is executed and control passes to the trap.
*      if 3 or more fatal errors occur an abort is signalled
*      regardless of errlimit and setexit - looping is all too
*      probable otherwise. fatal errors include stack overflow
*      and exceeding stlimit.
{err05{ssl{3,iniss{{{restore main prog s-r stack ptr{29303
{{bnz{3,dmvch{6,err08{{jump if in mid-dump{29304
*      merge here from err08 and err04 (error 320)
{err06{bze{3,kverl{6,labo1{{abort if errlimit is zero{29308
{{jsr{6,ertex{{{get fail message text{29309
*      merge from err04
{err07{bge{3,errft{18,=num03{6,labo1{abort if too many fatal errors{29313
{{dcv{3,kverl{{{decrement errlimit{29314
{{mov{7,xl{3,r_ert{{load errtype trace pointer{29315
{{jsr{6,ktrex{{{generate errtype trace if required{29316
{{mov{8,wa{3,r_cod{{get current code block{29317
{{mov{3,r_cnt{8,wa{{set cdblk ptr for continuation{29318
{{scp{8,wb{{{current code pointer{29319
{{sub{8,wb{8,wa{{offset within code block{29320
{{mov{3,stxoc{8,wb{{save code ptr offset for scontinue{29321
{{mov{7,xr{3,flptr{{set ptr to failure offset{29322
{{mov{3,stxof{9,(xr){{save failure offset for continue{29323
{{mov{7,xr{3,r_sxc{{load setexit cdblk pointer{29324
{{bze{7,xr{6,lcnt1{{continue if no setexit trap{29325
{{zer{3,r_sxc{{{else reset trap{29326
{{mov{3,stxvr{21,=nulls{{reset setexit arg to null{29327
{{mov{7,xl{9,(xr){{load ptr to code block routine{29328
{{bri{7,xl{{{execute first trap statement{29329
*      interrupted partly through a dump whilst store is in a
*      mess so do a tidy up operation. see dumpr for details.
{err08{mov{7,xr{3,dmvch{{chain head for affected vrblks{29334
{{bze{7,xr{6,err06{{done if zero{29335
{{mov{3,dmvch{9,(xr){{set next link as chain head{29336
{{jsr{6,setvr{{{restore vrget field{29337
*      label to mark end of code
{s_yyy{brn{6,err08{{{loop through chain{29341
{{ttl{27,s p i t b o l -- here endeth the code{{{{29342
*      end of assembly
{{end{{{{end macro-spitbol assembly{29346

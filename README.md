# Cobol unit testing framework for batch programs

This is a unit testing framework for batch Cobol programs. The goal is to enable isolated unit testing of individual paragraphs in Cobol programs in a standalone environment with no connection to a zOS system. 

This setup depends on [GNU Cobol](http://sourceforge.net/projects/open-cobol/).

## Setup

If you are loading this on a system configured using https://github.com/neopragma/provision-cobol-dev-ubuntu, then clone this repo as follows:

```shell
cd ~/projects
clone https://github.com/neopragma/cobol-unit-test
```

Run the build script like this:

```shell
cd ~/projects/cobol-unit-test
source envvars
build
``` 

If you are configuring the software yourself, then start by installing [GNU Cobol](http://sourceforge.net/projects/open-cobol/). I haven't set this up on any other platforms, so I can't help you with configuration issues that may arise.

## Project context

People might argue that we can use Cobol differently than we do in this project. Here is some context to explain why we aren't doing so.

### We're not talking about Object Cobol

There is such a thing as [object-oriented Cobol](http://www.cobolstandard.info/wg4/wg4.html). In fact, several implementations are available. However, our goal with this project is to support automated unit testing and test-driven development for existing, "legacy" Cobol applications running on IBM mainframes. In that environment, object-oriented Cobol is not widely used. In this document, the term "object-oriented language" excludes object-oriented Cobol by definition.

### We're not talking about *nix and Windows development

GNU Cobol is well-suited to *nix and Windows software development. This project is based on GNU Cobol, but its goal is not to target *nix and Windows systems. We aim to provide tools for mainframe programmers to enable offline automated unit testing and test-driven development of applications that will be hosted on a mainframe system. Compatibility with legacy mainframe Cobol syntax and style is key. Therefore, features of GNU Cobol that are compelling in their own right, but that are not compatible with mainframe Cobol, are not of interest for this project. In fact, the reason to use GNU Cobol for this project is mainly that Cobol support in the [Z390 Portable Mainframe Assembler and Emulator](www.z390.org) project doesn't seem to be fully baked. 

## Culture and automated unit testing in Cobol

The first challenge to meet when adopting practices like unit testing and test-driven development (TDD) for Cobol is not technical, but cultural. Historically, the Cobol community has not adopted these practices. As the competitive business environment continues to call for ever-shorter time to market, we are finding it necessary to bring existing Cobol applications into the world of frequent check-in cycles, continuous integration, and continuous delivery. The old school mindset that accepts long lead times is no longer adequate. Assuming we can cross the cultural barrier, what technical challenges do we face to enable rapid development cycles for Cobol applications?

The first step toward unit testing is for Cobol programmers to think about testing smaller chunks of code than they have historically done. Come to think of it, many Cobol programmers do not understand that testing is part of their job at all; so thinking about smaller chunks may be the _second_ step. In any case, it's a pretty early step. 

## Technical differences

Let's examine some of the differences between legacy Cobol and object-oriented languages that make it challenging to come up with a practical unit testing framework. 

### Cobol doesn't really have variables

A Cobol program looks at memory differently than object-oriented languages in that (on the mainframe) it does not use a stack and heap. Data Division definitions are unlike variable declarations in C or Java; they are more like peep-holes into a contiguous series of virtual memory addresses. A definition like this:

```
        WORKING-STORAGE SECTION.
        . . .
           10  PHONE-NUMBER         PIC X(10) VALUE LOW-VALUES.
        . . .   
```

is a shorthand way of saying, &quot;10 bytes of memory starting at the offset from the beginning of Working Storage that is represented by the token ```PHONE-NUMBER```.&quot; The PIC (short for PICTURE) clause specifying &quot;X&quot; tells the compiler to expect character data in this segment of memory, so that when our code refers to the token PHONE-NUMBER, the compiler will generate appropriate machine instructions for manipulating character data. This is not enforced, however. The VALUE clause tells the compiler to generate code that initializes this segment of memory to binary zeroes (&quot;LOW-VALUES&quot;) when the program starts. 

We loosely refer to ```PHONE-NUMBER``` as a &quot;variable,&quot; but it's really just a label for a series of contiguous virtual memory locations at a certain offset from a known starting address, plus a few hints to the compiler. &quot;Variables&quot; in Cobol are really _fields_, or regions of memory. (Note: We also loosely refer to variables as &quot;fields&quot; in languages where they are _not_ peep-holes into a contiguous series of virtual memory addresses. Go figure.)

Object-oriented languages work differently. For instance, in Java, this sort of declaration has a very different meaning:

```
           protected String phoneNumber = null;
```

This declares a name that represents a _reference_ (an object that contains a pointer to another object) to an object of class ```java.lang.String```. The location of the object in memory and how the Java runtime manages it are not visible to the programmer. In fact, due to string interning, many variables in many objects may refer to the same String object in memory. Conversely, the same reference may point to different Strig objects at different points during a program's execution. The sample declaration assigns an initial value of ```null``` to the reference, which means it does not point to an object (its pointer contains binary zeroes).

In Cobol, there is no reference object and no pointer. ```PHONE-NUMBER``` is just a chunk of virtual storage. Its virtual storage address remains the same as long as the program is loaded in memory.

### In Cobol, all references to data are global

In the example above, the Java ```protected``` keyword specifies that the variable ```phoneNumber``` is visible to code that resides in the same class as the one in which it is declared, in any of its subclasses, and in any classes in the same _package_ (a way of grouping related Java classes together for deployment). The variable cannot be referenced by other code in the application. Doing so causes a compile-time error.

All changes to data in the Data Division of a Cobol program affect the entire program. There is no equivalent to the concepts of _scope_ or _visibility_ in Cobol. You can't declare a local variable inside a paragraph. Everything in the program is visible to all parts of the program. Except for Object Cobol (and very little legacy code was written in Object Cobol), you cannot subclass a Cobol program to override a single paragraph, as we sometimes do for test purposes in object-oriented languages.

### Cobol doesn't have objects or classes

The [String class in Java](http://docs.oracle.com/javase/7/docs/api/java/lang/String.html) encapsulates functionality relevant to manipulating string-type data. It's the same with other object-oriented languages, like [C#](http://msdn.microsoft.com/en-us/library/system.string%28v=vs.110%29.aspx), [Objective C](https://developer.apple.com/library/mac/documentation/Cocoa/Reference/Foundation/Classes/NSString_Class/), [C++](http://www.cplusplus.com/reference/string/string/), [Smalltalk](https://www.gnu.org/software/smalltalk/manual-base/html_node/String.html), [Ruby](http://www.ruby-doc.org/core-2.1.5/String.html), [Python](https://docs.python.org/2/library/string.html), [Eiffel](http://smarteiffel.loria.fr/libraries/api/lib.d/string.d/loadpath.se.d/STRING/ANY.html), [Erlang](http://www.erlang.org/doc/man/string.html), and...well, [_all_ of them](http://en.wikipedia.org/wiki/Comparison_of_programming_languages_%28string_functions%29).

For instance, to find out if the string "John Smith" contains the substring "Sm", you can write the following Java code

```java
    String name = "John Smith";
    boolean result = name.contains("Sm");
```

or the following Ruby code

```ruby
    name = "John Smith"
    result = name.include? "Sm"
```

or the following Smalltalk code

```smalltalk
    name := "John Smith"
    result := name includesString: "Sm"
```

and so on using any object-oriented language. To do the same thing with Cobol, you'd have to write a small routine to iterate over the field and look for the substring.

The Cobol Data Division entry with the label ```PHONE-NUMBER``` doesn't know what character-type data is and doesn't know what a "phone number" is. There's no such thing as ```PHONE-NUMBER.INCLUDES```. It's just an offset and length of a chunk of memory that has been allocated to the program. You can declare it as PIC X but nothing prevents your moving numerical or binary data into it. You can move too much data into it and overwrite whatever happens to follow it in memory. There's almost no built-in protection against populating a field with invalid data, and very little built-in support for manipulating data values.

### A Cobol paragraph is not a function or method

As a general rule in software development, a &quot;unit test&quot; exercises a single path through a single _unit_ of code, where &quot;unit&quot; refers to the smallest meaningful chunk of code the particular programming language defines. In C, the smallest meaningful chunk of code is a _function_. In Java, the smallest meaningful chunk of code is a _method_. So, a unit test for a C program would exercise a single path through a single function, and a unit test for a Java class would exercise a single path through a single method. If the method can have valid vs. invalid arguments, or if it contains conditional logic, or an exception is possible, then more than one unit test will be needed to exercise all execution paths.

In Cobol, the conceptual equivalent of a function or method is the _paragraph_. Therefore, we want to be able to test individual paragraphs in a Cobol program. But Cobol works quite differently from C or Java. A paragraph may be _conceptually_ similar to a method, but it is not _implemented_ like a method. 

A paragraph is a procedural block. It does not take arguments and does not return a value. It may modify data defined in the Data Division, but does not handle input and output values in the same way as a function or method.

## Organizing Cobol code to enable automated unit testing

If we want to run automated unit tests against a Cobol program, we need a way to isolate the individual paragraphs containing logic that is worthwhile to test at the &quot;unit&quot; level. Of course, we will also test the overall functionality of the program as a whole, in its normal runtime context. One level of automated testing does not obviate the need for other levels of automated testing. 

Look for the sample program, ```CONVERT.CBL```. The purpose of the program is to convert an input file into a different format and write the converted data to an output file. It is a simplified example of a very common type of batch program in mainframe environments. 

Three versions of the program are provided. ```CONVERT-BAD.CBL``` is organized in the same way as many legacy Cobol programs: All the logic of the program is located in the same paragraph. This sort of design is sometimes called _spaghetti code_. To test this paragraph as a &quot;unit&quot; would be no different than testing the whole program, as there is no way to execute any subset of the logic in isolation. We can only test the whole program, which would be a &quot;functional&quot; or &quot;integration&quot; test, and not a &quot;unit&quot; test.

The second version is called ```CONVERT-BAD2.CBL```. This version is the same as ```CONVERT-BAD.CBL``` except that the logically-distinct segments of functionality are visually separated by comments. This version also can't be unit tested, paragraph by paragraph, but the comments help us to see where we could tease the code apart into separate paragraphs. Many legacy programs contain comments like this to help programmers understand the functionality of the code. 

The logical points in source code where we can separate chunks of code without breaking the functionality are called _seams_. The analogy is with the seams in your clothing, which are the easiest places to pull the clothing apart. (Note: It's only an analogy. In most cases, it is more desirable to pull your monolithic code apart than it is to pull your clothing apart.)

The third version is called ```CONVERT.CBL```. This version illustrates the way to organize source code so that the individual paragraphs can be unit tested in isolation, without having to set up a full run of the program with real datasets. Input/output processing and initialization logic are separated from &quot;business logic.&quot; Small, discrete pieces of functionality are coded in separate paragraphs. Those paragraphs have no runtime dependencies on anything external to the program. This allows a unit test case to perform a single paragraph without requiring all the program's runtime dependencies to be in place.

Some people who are new to the idea of unit testing worry that a multitude of tiny paragraphs will cause performance problems compared with inline code. There is no need for worry, as the Cobol optimizer is very mature and will inline the paragraphs appropriately. For purposes of human readability and code testability, it's more important to keep the distinct chunks of logic nicely separated than it is to try and hand-optimize the source code for runtime performance. (Note: Yes, you are a highly skilled programmer. No, you will not be able to hand-optimize your code more effectively than the optimizer. Don't waste your time trying.)

Before you can start writing unit tests against legacy Cobol programs, it's possible you will have to reorganize the code in a way similar to changing ```CONVERT-BAD.CBL``` into ```CONVERT.CBL```. This sort of modification, which changes the internal structure of code without changing its behavior, is called _refactoring_. The analogy is with algebra, in which we can refactor expressions in ways that change their form without changing their meaning.

## How this unit testing framework works

Cobol was not designed to allow for modification of object code after compilation. To do &quot;special&quot; things with Cobol code, such as injecting unit test cases, we have to modify the source before running the compiler. You are probably familiar with preprocessors that convert EXEC CICS and EXEC SQL commands into subroutine calls before the compiler step in your compile procs. This is exactly the same idea. 

The precompiler copies the program to be tested, just in case there's an error that corrupts the code. While copying, the precompiler keeps an eye out for the ```WORKING-STORAGE SECTION``` header and the ```PROCEDURE DIVISION``` header. It adds a ```COPY``` statement at each of those locations in the source code.

At the top of the ```WORKING-STORAGE SECTION```, the precompiler inserts a ```COPY``` statement that includes code like the following:

```
      * TEST CODE INSERTED BY ZUTZCPC
       77  UT-EXPECTED                   PIC X(25) VALUE SPACES.
       77  UT-ACTUAL                     PIC X(25) VALUE SPACES.
       77  UT-FAILED                     PIC X(11)  VALUE "**** FAIL: ".
       77  UT-PASSED                     PIC X(11)  VALUE "     PASS: ".
       77  UT-TEST-CASE-NAME             PIC X(80)  VALUE SPACES.
       77  UT-RETCODE                    PIC 9(4)   VALUE ZERO.
      * END OF TEST CODE 
```

These are ```WORKING-STORAGE``` items that are used only by the unit test code. If the names conflict with names already in use in the program under test, or if you just prefer different names or you want to add more fields, you can change the contents of the copybook. In fact, you will probably want to make the ```UT-EXPECTED``` and ```UT-ACTUAL``` fields longer, and/or add fields to handle different types of data or more-complex results.

At the top of the ```PROCEDURE DIVISION```, the precompiler inserts a ```COPY``` statement that includes code like the following:

```
      * UNIT TESTS FOR SAMPLE.CBL
           
           DISPLAY SPACES
           DISPLAY '==================================================='
           DISPLAY 'UNIT TESTS FOR SAMPLE.CBL'

           MOVE 'IT RETURNS HELLO, WORLD! AS GREETING' 
               TO UT-TEST-CASE-NAME
           MOVE 'HELLO, WORLD!' TO UT-EXPECTED
           MOVE 'GREETING' TO WS-MESSAGE-TYPE
           PERFORM 2000-SPEAK
           MOVE WS-MESSAGE TO UT-ACTUAL
           PERFORM UT-CHECK

           MOVE 'IT RETURNS GOODBYE, CRUEL WORLD! AS FAREWELL' 
               TO UT-TEST-CASE-NAME
           MOVE 'GOODBYE, CRUEL WORLD!' TO UT-EXPECTED
           MOVE 'FAREWELL' TO WS-MESSAGE-TYPE
           PERFORM 2000-SPEAK
           MOVE WS-MESSAGE TO UT-ACTUAL
           PERFORM UT-CHECK

           DISPLAY '==================================================='

           MOVE UT-RETCODE TO RETURN-CODE
           GOBACK
           .
       UT-CHECK.
           IF UT-ACTUAL IS EQUAL TO UT-EXPECTED
               DISPLAY UT-PASSED UT-TEST-CASE-NAME
           ELSE 
               DISPLAY UT-FAILED UT-TEST-CASE-NAME 
               DISPLAY '    EXPECTED <' UT-EXPECTED '>'
               DISPLAY '      ACTUAL <' UT-ACTUAL '>'
               MOVE 4 TO UT-RETCODE
           END-IF             
           .     
       UT-CHECK-END.
           .       
```

This is what the unit tests look like. It's all based on coding conventions. There's no compile-time enforcement of the conventions.

Unit tests follow the same structure as they do in other languages:

* Set the preconditions for the test case
* Execute the code under test
* Compare the actual results with the expected results

In the sample, this sets the expected result for the first test case:

```
           MOVE 'HELLO, WORLD!' TO UT-EXPECTED
```

This sets the preconditions for the test case by modifying the contents of ```WORKING-STORAGE```:

```
           MOVE 'GREETING' TO WS-MESSAGE-TYPE
```

This executes the code under test (in this case, just the paragraph ```2000-SPEAK```, and not the rest of the program):

```
           PERFORM 2000-SPEAK
```

This copies the result (a field i ```WORKING-STORAGE```) to the ```UT-ACTUAL``` field (part of the unit test framework) so that paragraph ```UT-CHECK``` can compare it to the expected output in ```UT-EXPECTED```, and then performs ```UT-CHECK```:

```
           MOVE WS-MESSAGE TO UT-ACTUAL
           PERFORM UT-CHECK
```

The same pattern can be repeated as many times as necessary to provide adequate unit test coverage. There is no need to create and populate files or databases or to have live connectivity to external systems. You just set up ```WORKING-STORAGE``` the way it needs to look and perform a paragraph. 

Here are the salient characteristics of a unit test copybook:

* End the unit test code with a ```GOBACK```. You don't want the program to execute from start to finish. You only want to invoke selected paragraphs. This way, you can avoid performing any paragraphs that contain I/O calls.
* In the sample, the first and last things to be displayed provide a visual indication of where the unit test output begins and ends. You can display whatever is relevant in your environment, if anything. Use ```UT-TEST-CASE-NAME``` to make it clear which tests were executed and which ones passed.
* When running the unit tests on-platform, you can direct the output from the ```DISPLAY``` statements to a file by including an appropriate ``DD`` statement in the ```JCL```. When you run the unit tests off-platform (say, on your laptop), then the test output goes to the standard output stream. You can redirect it to a file, if you wish.
* You can code as many unit test cases as you need in the same copybook. You can also group logically-related test cases in separate copybooks. The precompiler will only insert a single ```COPY``` statement, but you can nest ```COPY``` statements as needed.
* Set ```UT-RETCODE``` based on the results of the test cases. Remember to move ```UT-RETCODE``` to ```RETURN-CODE``` just prior to the ```GOBACK``` statement, so the system condition code will be set appropriately. 

To automate unit tests for an entire application on-platform, you can create a multi-step jobstream and test the condition code after each step. By convention, condition code 0 denotes success and condition code 4 means one or more unit test cases failed. When running the unit tests off-plaform, you can check the status code using the shell language in which you wrote your test script. The unit testing framework is designed to work the same way on-platform and off-platform. Only the ```JCL```, shell language, or batch language is platform-dependent.

To run the unit tests, the build script executes the copy of the program that has the unit test cases copied into its Procedure Division.

### Isolation from the system date and time

There is often a need to set up test cases that depend on a specific date and time. When the code under test obtains the current date and time from the system, these tests are fragile because the date and time values will be different in each run. We need a way to set a specific system date and time so that the test cases will be repeatable. This is true for all programming languages, and is not a unique issue for testing Cobol programs.

For any programming language, it is common practice to refactor the production code so that it is possible to inject a specific date and time for the purpose of automated testing. We can do the same with Cobol programs.

Cobol applications may use any of several mechanisms to obtain the system date time or a run date from the runtime environment, including JCL statements, SYSIN, or an environment variable. To support isolation from the system date and time, we need to refactor the Cobol program we wish to test in the following ways:

* Code that references the system date and time (or run date) is isolated in a separate paragraph. Wherever the code had referenced the system date and time, it performs that paragraph instead.
* Add a working storage variable to control whether the date and time code actually sets the value from the system, or uses the values that are already present in working storage. These fields can then be set by unit test code.

There are several different ways programmers can obtain date and time values from the system. For new code, it's preferable to use the _intrinsic functions_ to obtain date and time values. In older legacy code, you will find various forms of the accept statement. For that reason, it isn't practical to write a one-size-fits-all parser to replace such code with alternative testable code. On a case by case basis, we have to write unit test code that sets the approprite values in working storage for each particular Cobol program we want to test. Mechanisms to obtain date and time values include:

* ```MOVE FUNCTION CURRENT-DATE to [variable]```
* ```MOVE FUNCTION SECONDS-PAST-MIDNIGHT to [variable]```
* ```ACCEPT [variable] FROM DATE```
* ```ACCEPT [variable] FROM DATE YYYYMMDD```
* ```ACCEPT [variable] FROM DAY```
* ```ACCEPT [variable] FROM DAY YYYYMMDD```
* ```ACCEPT [variable] FROM DAY-OF-WEEK```
* ```ACCEPT [variable] FROM TIME```

We can refactor code that uses these mechanisms to perform a separate paragraph instead, like this:

```
      *    (original)
           ACCEPT [variable] FROM DATE YYYYMMDD

      *    (refactored)
           PERFORM GET-CURRENT-DATE
    . . .
       GET-CURRENT-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS
           .
```

Coded this way, the program will still obtain the system date every time the paragraph is called. If we add a Working Storage item to control this, we can modify the code like so:

```
       01  FILLER                PIC X VALUE 'Y'.
           88  SET-CURRENT-DATE        VALUE 'Y'.
           88  DO-NOT-SET-CURRENT-DATE VALUE 'N'. 
    . . .
       GET-CURRENT-DATE.
           IF  SET-CURRENT-DATE
               MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS
           END-IF
           .
```

Now our unit test code can set up the preconditions for a date/time-dependent test case by setting the current date and time fields in Working Storage to whatever values we want. This means our test will be _repeatable_, an absolute requirement for reliable automated tests. 

## Automated functional testing

The framework doesn't address the case when we want to test a single step (executing a Cobol program) from a jobstream. It turns out to be fairly straightforward to set up test jobstreams using IBM utilities, in particular [SuperC](http://www-01.ibm.com/support/knowledgecenter/SSLTBW_2.1.0/com.ibm.zos.v2r1.f54u200/chap8.htm). This enables us to test single job steps in isolation on-platform.

If you need to automate the same level of testing off-platform, you can create fake input files and &quot;expected&quot; output files for your program, and then write a test script that populates the files appropriately, executes your program, and compares the actual and expected output files. You can use a ```diff``` utility or roll your own compare program or script. So, there's no need for a special framework to support functional testing.

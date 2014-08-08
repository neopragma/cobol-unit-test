# Cobol unit testing framework for batch programs

This is a unit testing framework for batch Cobol programs. The goal is to enable isolated unit testing of individual paragraphs in Cobol programs in a standalone environment with no connection to a zOS system. In addition, it supports functional testing of batch programs in a standalone environment.

This setup depends on GNU Cobol.

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

## Unit testing challenges for Cobol

The first challenge to meet when adopting practices like unit testing and test-driven development (TDD) for Cobol is not technical, but cultural. Historically, the Cobol community has not adopted these practices. As the competitive business environment continues to call for ever-shorter time to market, we are finding it necessary to bring existing Cobol applications into the world of frequent check-in cycles, continuous integration, and continuous delivery. The old school mindset that accepts long lead times is no longer adequate. Assuming we can cross the cultural barrier, what technical challenges do we face to enable rapid development cycles for Cobol applications?

The first step toward unit testing is for Cobol programmers to think about testing smaller chunks of code than they have historically done. Come to think of it, many Cobol programmers do not understand that testing is part of their job at all; so thinking about smaller chunks may be the _second_ step. In any case, it's a pretty early step. 

As a general rule in software development, a &quot;unit test&quot; exercises a single path through a single _unit_ of code, where &quot;unit&quot; refers to the smallest meaningful chunk of code the particular programming language defines. In C, the smallest meaningful chunk of code is a _function_. In Java, the smallest meaningful chunk of code is a _method_. So, a unit test for a C program would exercise a single path through a single function, and a unit test for a Java class would exercise a single path through a single method. If the method can have valid vs. invalid arguments, or if it contains conditional logic, then more than one unit test will be needed to exercise all execution paths.

In Cobol, the conceptual equivalent of a function or method is the _paragraph_. Therefore, we want to be able to test individual paragraphs in a Cobol program. But Cobol works quite differently from C or Java. A paragraph may be _conceptually_ similar to a method, but it is not _implemented_ like a method. Let's examine some of the differences. 

A Cobol program looks at memory differently than other languages in that it does not use a stack and heap. Data Division definitions are unlike variable declarations in C or Java; they are more like windows into a contiguous series of virtual memory addresses. A definition like this:

```
        working-storage section.
        . . .
           10  phone-number         pic x(10) value low-values.
        . . .   
```

is a shorthand way of saying, &quot;10 bytes of memory starting at the offset from the beginning of Working Storage that is represented by the token _phone-number_. The _pic_ (short for _picture_) clause specifying &quot;x&quot; tells the compiler to expect character data in this segment of memory, so that when our code refers to the token _phone-number_ the compiler will generate appropriate machine instructions for manipulating character data. The _value_ clause tells the compiler to generate code that initializes this segment of memory to binary zeroes (&quot;low-values&quot;) when the program starts. 

We loosely refer to ```phone-number``` as a &quot;variable,&quot; but it is really just a label for a series of contiguous virtual memory locations at a certain offset from a known starting address, plus a few hints to the compiler. &quot;Variables&quot; in Cobol are really _fields_, or regions of memory. (Note: We also loosely refer to variables as &quot;fields&quot; in languages where they are _not_ windows into a contiguous series of virtual memory addresses. Go figure.)

In Java, this sort of declaration has a very different meaning:

```
           protected String phoneNumber = null;
```

This declares a variable name that represents a _reference_ (sort of a pointer thingy, don't sweat the details) to an object of class ```java.lang.String```. The location of the object in memory and how the Java runtime manages it are not visible to the programmer. The declaration assigns an initial value of ```null``` to the reference, which means it does not point to an object (its pointer contains binary zeroes).

Furthermore, the Java ```protected``` keyword specifies that the variable ```phoneNumber``` is visible to code that resides in the same class as the one in which it is declared, in any of its subclasses, and in any classes in the same _package_ (a way of grouping related Java classes together for deployment). The variable cannot be referenced by other code in the application. Cobol has no equivalent functionality.

All changes to data in the Data Division of a Cobol program affect the entire program. There is no equivalent to the concepts of _scope_ or _visibility_ in Cobol. You can't declare a local variable inside a paragraph. Everything in the program is visible to all parts of the program. Except for Object Cobol (and very little legacy code was written in Object Cobol), you cannot subclass a Cobol program to override a single paragraph, as we sometimes do for test purposes in object-oriented languages.

Finally, a paragraph does not take arguments and does not return a value. It may modify values in the Data Division, but does not handle input and output values in the same way as a function or method.

### Organizing Cobol code to enable automated unit testing

If we want to run automated unit tests against a Cobol program, we need a way to isolate the individual paragraphs containing logic that is worthwhile to test at the &quot;unit&quot; level. Of course, we will also test the overall functionality of the program as a whole, in its normal runtime context. One level of automated testing does not obviate the need for other levels of automated testing. 

In the ```file-converter``` sample project, you will find the sample program, ```convert.cbl```. The purpose of the program is to convert an input file into a different format and write the converted data to an output file. It is a simplified example of a very common type of batch program in mainframe environments. 

Three versions of the program are provided. ```convert-bad.cbl``` is organized in the same way as many legacy Cobol programs: All the logic of the program is located in the same paragraph. This sort of design is sometimes called _spaghetti code_. To test this paragraph as a &quot;unit&quot; would be no different than testing the whole program, as there is no way to execute any subset of the logic in isolation. We can only test the whole program, which would be a &quot;functional&quot; or &quot;integration&quot; test, and not a &quot;unit&quot; test.

The second version is called ```convert-bad2.cbl```. This version is the same as ```convert-bad.cbl``` except that the logically-distinct segments of functionality are visually separated by comments. This version also can't be unit tested, paragraph by paragraph, but the comments help us to see where we could tease the code apart into separate paragraphs. Many legacy programs contain comments like this to help programmers understand the functionality of the code. 

The logical points in source code where we can separate chunks of code without breaking the functionality are called _seams_. The analogy is with the seams in your clothing, which are the easiest places to pull the clothing apart. (Note: It's only an analogy. In most cases, it is more desirable to pull your monolithic code apart than it is to pull your clothing apart.)

The third version is called ```convert.cbl```. This version illustrates the way to organize source code so that the individual paragraphs can be unit tested in isolation, without having to set up a full run of the program with real datasets. Input/output processing and initialization logic are separated from &quot;business logic.&quot; Small, discrete pieces of functionality are coded in separate paragraphs. Those paragraphs have no runtime dependencies on anything external to the program. This allows a unit test case to perform a single paragraph without requiring all the program's runtime dependencies to be in place.

Some people who are new to the idea of unit testing worry that a multitude of tiny paragraphs will cause performance problems compared with inline code. There is no need for worry, as the Cobol optimizer is very mature and will inline the paragraphs appropriately. For purposes of human readability and code testability, it's more important to keep the distinct chunks of logic nicely separated than it is to try and hand-optimize the source code for runtime performance. (Note: Yes, you are a highly skilled programmer. No, you will not be able to hand-optimize your code more effectively than the optimizer. Don't waste your time trying.)

Before you can start writing unit tests against legacy Cobol programs, it's possible you will have to reorganize the code in a way similar to changing ```convert-bad.cbl``` into ```convert.cbl```. This sort of modification, which changes the internal structure of code without changing its behavior, is called _refactoring_. The analogy is with algebra, in which we can refactor expressions in ways that change their form without changing their meaning.

### How this unit testing framework works

Cobol was not designed to allow for modification of object code after compilation. To do &quot;special&quot; things with Cobol code, such as injecting unit test cases, we have to modify the source before running the compiler. You are probably familiar with preprocessors that convert EXEC CICS and EXEC SQL commands into subroutine calls before the compiler step in your compile procs. This is exactly the same idea. 

For unit testing, the ```cobuild``` script copies the source program to a temporary file and adds test code to that file in a preprocessor step. We follow some conventions to keep the process relatively simple. Take a look at ```projects/file-converter/src/test/cobol/unit-tests/convert-unit-tests``` to see how a unit test copybook is structured. 

Unit test cases are coded in a copybook. The build script inserts a copy statement immediately below the Procedure Division header in the temporary copy of the program to inject the unit test cases into the program under test. It also inserts a few 77-level item in the Working Storage Section. One of these is named ```retcode```. Unit test code sets ```retcode``` to a non-zero value when an assertion fails. The unit test code in the copybook must end by moving ```retcode``` to ```return-code``` and then doing a ```goback```. That way, the build script can react to different exit codes.

To run the unit tests, the build script executes the copy of the program that has the unit test cases copied into its Procedure Division.

Each unit test case sets up the test by setting values in the Working Storage Section. Then it performs one or more paragraphs in the program under test, but does not execute any of the usual initialization logic that depends on the runtime environment. Finally, it compares values in Working Storage with the values it expects to find when the paragraph functions correctly. When the comparison fails, the test case sets a non-zero value in ```retcode```, but does not terminate. That way, all the test cases can run and we can see the status of all of them.

The ```cobuild``` script inserts a single copy statement only. It does not support the insertion of multiple copy statements. If you need to organize a large number of test cases for ease of maintenance or you prefer to group logically-related test cases for ease of comprehension, you can nest copy statements within the main unit test file.

With the copy code in place, when the program is executed it doesn't run normally. Instead, the unit test cases at the top of the Procedure Division run straight through to the ```goback``` at the end of the included code. The unit tests perform individual paragraphs in the program without opening any files or accessing any other external system resources. Output from the test cases goes to standard output via ```display``` statements. 

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
<pre class="code box">
      *    (original)
           ACCEPT [variable] FROM DATE YYYYMMDD

      *    (refactored)
           PERFORM GET-CURRENT-DATE
    . . .
       get-current-date.
           move function current-date to ws-current-date-fields
           .
```

Coded this way, the program will still obtain the system date every time the paragraph is called. If we add a Working Storage item to control this, we can modify the code like so:

```
       01  filler                pic x value 'Y'.
           88  set-current-date        value 'Y'.
           88  do-not-set-current-date value 'N'. 
    . . .
       get-current-date.
           if  set-current-date
               move function current-date to ws-current-date-fields
           end-if
           .
```

Now our unit test code can set up the preconditions for a date/time-dependent test case by setting the current date and time fields in Working Storage to whatever values we want. This means our test will be _repeatable_, an absolute requirement for reliable automated tests. See ```cobol-unit-test/src/test/unit-tests/invdate-unit-tests``` for the complete code.

## Integration (functional) tests

In addition to unit testing, this framework supports integration or functional testing of the whole Cobol program in an isolated environment. The ```cobuild``` script will run whatever executable you tell it to run in the ```--integration``` command line argument. You prepare a custom integration test driver that does whatever has to be done to run the Cobol program you want to test. It can be a program written in any programming language or a shell script. 

This is much easier to set up than the unit tests, because we need not preprocess the Cobol source code before compiling the program. In the sample project ```file-converter```, a shell script sets up an input file and an expected output file, then runs the program under test, and finally runs another Cobol program whose purpose is to compare the contents of the expected and actual output files.

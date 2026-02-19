# Report for Assignment 3

This is a template for your report. You are free to modify it as needed. It is
not required to use Markdown, but the report has to be delivered in a standard,
cross-platform format.

## Project

Name: Apache Commons Lang

URL: https://github.com/apache/commons-lang

Apache Commons Lang is a Java library that adds extra utility methods on top of the standard java.lang API. It covers things like string manipulation, basic math, object reflection, and system properties, and is maintained by the Apache Software Foundation.

## Onboarding Experience

Did it build and run as documented?

See the assignment for details; if everything works out of the box, there is no
need to write much here. If the first project(s) you picked ended up being
unsuitable, you can describe the onboarding experience for each project, along
with reasons why you changed to a different one.

1. **How easily can you build the project? Briefly describe if everything worked
   as documented or not.**

   **(a) Did you have to install a lot of additional tools to build the
   software?**

   Not really. Since the group has worked with Java and Maven before, we already
   had the necessary tools installed.

   **(b) Were those tools well documented?**

   N/A.

   **(c) Were other components installed automatically by the build script?**

   Many dependencies were installed automatically by Maven.

   **(d) Did the build conclude automatically without errors?**

   Since a `report.md` was included in the project and the recommended command
   to check that all tests and checks pass is `mvn`, the following error
   occurred:

   ```bash
   Assignment-3-CodeComplex-Coverage % mvn
   [INFO] Scanning for projects...
   [INFO]
   [INFO] ------------------< org.apache.commons:commons-lang3 >------------------
   [INFO] Building Apache Commons Lang 3.21.0-SNAPSHOT
   [INFO]   from pom.xml
   [INFO] --------------------------------[ jar ]---------------------------------
   [INFO]
   [INFO] --- clean:3.5.0:clean (default-clean) @ commons-lang3 ---
   [INFO]
   [INFO] --- enforcer:3.6.2:enforce (enforce-maven-version) @ commons-lang3 ---
   [INFO] Rule 0: org.apache.maven.enforcer.rules.version.RequireMavenVersion passed
   [INFO]
   [INFO] --- enforcer:3.6.2:enforce (enforce-java-version) @ commons-lang3 ---
   [INFO] Rule 0: org.apache.maven.enforcer.rules.version.RequireJavaVersion passed
   [INFO]
   [INFO] --- apache-rat:0.17:check (rat-check) @ commons-lang3 ---
   [WARNING] Basedir is : Assignment-3-CodeComplex-Coverage
   [INFO] Excluding patterns: site-content/**, src/site/resources/.htaccess,
   src/site/resources/download_lang.cgi, src/site/resources/release-notes/RELEASE-NOTES-*.txt,
   src/test/resources/lang-708-input.txt, **/*.svg, **/*.xcf, site-content/**,
   .checkstyle, .fbprefs, .pmd, .asf.yaml, .gitattributes, src/site/resources/download_*.cgi,
   maven-eclipse.xml, .externalToolBuilders/**, .vscode/**, .project, .classpath,
   .settings/**, **/*.svg, **/*.xcf
   [INFO] Excluding MAVEN collection.
   [INFO] Excluding ECLIPSE collection.
   [INFO] Excluding IDEA collection.
   [INFO] Processing exclude file from STANDARD_SCMS.
   [INFO] Excluding STANDARD_SCMS collection.
   [INFO] Excluding MISC collection.
   [INFO] RAT summary:
   [INFO]   Approved:  573
   [INFO]   Archives:  0
   [INFO]   Binaries:  4
   [INFO]   Document types:  4
   [INFO]   Ignored:  36
   [INFO]   License categories:  2
   [INFO]   License names:  2
   [INFO]   Notices:  3
   [INFO]   Standards:  574
   [INFO]   Unapproved:  1
   [INFO]   Unknown:  1
   [ERROR] Unexpected count for UNAPPROVED, limit is [0,0].  Count: 1
   [INFO] UNAPPROVED (Unapproved) is a count of unapproved licenses.
   [WARNING] *****************************************************
   Generated at: 2026-02-13T15:51:12+01:00

   Files with unapproved licenses:
     /report.md
   [INFO] ------------------------------------------------------------------------
   [INFO] BUILD FAILURE
   [INFO] ------------------------------------------------------------------------
   [INFO] Total time:  5.158 s
   [INFO] Finished at: 2026-02-13T15:51:16+01:00
   [INFO] ------------------------------------------------------------------------
   [ERROR] Failed to execute goal org.apache.rat:apache-rat-plugin:0.17:check (rat-check)
   on project commons-lang3: Counter(s) UNAPPROVED exceeded minimum or maximum values.
   See RAT report in: 'Assignment-3-CodeComplex-Coverage/target/rat.txt'. -> [Help 1]
   [ERROR]
   [ERROR] To see the full stack trace of the errors, re-run Maven with the -e switch.
   [ERROR] Re-run Maven using the -X switch to enable full debug logging.
   [ERROR]
   [ERROR] For more information about the errors and possible solutions, please read
   the following articles:
   [ERROR] [Help 1] http://cwiki.apache.org/confluence/display/MAVEN/MojoFailureException
   ```

   This error is caused by `org.apache.rat:apache-rat-plugin`, which audits the
   licenses of files in the repository. Since `report.md` is not licensed, the
   build fails. To fix this, the following line was added to the `pom.xml` in the
   RAT exclude list:

   ```xml
   <plugin>
     <groupId>org.apache.rat</groupId>
     <artifactId>apache-rat-plugin</artifactId>
     <configuration>
       <inputExcludes>
         <inputExclude>**/report.md</inputExclude>
         <inputExclude>site-content/**</inputExclude>
         <inputExclude>src/site/resources/.htaccess</inputExclude>
         <inputExclude>src/site/resources/download_lang.cgi</inputExclude>
         <inputExclude>src/site/resources/release-notes/RELEASE-NOTES-*.txt</inputExclude>
         <inputExclude>src/test/resources/lang-708-input.txt</inputExclude>
         <inputExclude>**/*.svg</inputExclude>
         <inputExclude>**/*.xcf</inputExclude>
       </inputExcludes>
     </configuration>
   </plugin>
   ```

   This excludes `report.md` from the license check and allows the build to pass.

   **(e) How well do examples and tests run on your system(s)?**

   Running the full build (tests and checks) took 18:52 minutes to complete.

2. **Do you plan to continue or choose another project?**

## Complexity

### Function: `StringUtils::splitWorker`

**Lizard tool results:**

| Overload | NLOC | CCN (lizard) | Lines |
|----------|------|--------------|-------|
| `String, char, boolean` | 32 | 10 | 7588-7620 |
| `String, String, int, boolean` | 78 | 23 | 7632-7715 |

**Manual CC count:**

Using the formula: **CC = (decision points) - (exit points) + 2**

splitWorker(String, char, boolean) = 9 - 3 + 2 = 8 (Lizard CC = 10)
splitWorker(String, String, int, boolean) = 22 - 3 + 2 = 21 (Lizard CC = 23)
**Questions:**

1. **Did tool and manual count get the same result?**  
   No. Lizard reports CC=10 and CC=23, while manual counts give CC=8 and CC=21. There could be multiple explanations for the difference of 2, such as lizard could be counting the while loop exit condition as an additional decision point, or maybe uses a different formula.
2. **Are the functions long as well as complex?**  
   Yes. The first overload is 32 NLOC with CC=10, and the second is 78 NLOC with CC=23. Both are longer than average functions.
3. **What is the purpose of the function?**  
   `splitWorker` splits a string into an array of substrings based on a separator. It provides the logic for `split()` and  `splitPreserveAllTokens` methods in `StringUtils`. The high CC comes from handling three cases: whitespace, single-char, multi-char separators.

4. **Are exceptions taken into account?**  
   No exceptions are thrown or caught in `splitWorker`. The function handles edge cases via early returns rather than exceptions.

5. **Is the documentation clear about all possible outcomes?**  
   The Javadoc describes the main parameters and return value, but does not explicitly document all branch outcomes. For example, the exact behavior when `max` is reached mid-string is missing.

## Refactoring

Plan for refactoring complex code:

Estimated impact of refactoring (lower CC, but other drawbacks?).

Carried out refactoring (optional, P+):

`git diff ...`

## Coverage

### Tools

Document your experience in using a new/different coverage tool. How well was
the tool documented? Was it possible, easy, or difficult to integrate it with
your build environment?

### Your Own Coverage Tool

Show a patch (or link to a branch) that shows the instrumented code to gather
coverage measurements. The patch is probably too long to be copied here, so
please add the git command used to obtain the patch instead:

`git diff ...`

What kinds of constructs does your tool support, and how accurate is its output?

### Evaluation

1. How detailed is your coverage measurement?
2. What are the limitations of your own tool?
3. Are the results of your tool consistent with existing coverage tools?

## Coverage Improvement

Show the comments that describe the requirements for the coverage.

Report of old coverage: [link]

Report of new coverage: [link]

Test cases added:

`git diff ...`

Number of test cases added: two per team member (P) or at least four (P+).

## Self-Assessment: Way of Working

Current state according to the Essence standard: ...

Was the self-assessment unanimous? Any doubts about certain items?

How have you improved so far?

Where is potential for improvement?

## Overall Experience

What are your main take-aways from this project? What did you learn?

Is there something special you want to mention here?

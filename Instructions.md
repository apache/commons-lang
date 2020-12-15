<h3>Instructions</h3>
Navigate to the commons-lang/src/main/java/org/apache/commons/lang3/math directory</br></br>

<b>Commands to compile for different tests:</b></br>
<li>javacheck -cp $ADDRESS -processor nullness,signedness,index,TaintingChecker Fraction.java</li></br>
<li>javacheck -cp $ADDRESS -processor org.checkerframework.common.value.ValueChecker -Astubs=statically-executable.astub Fraction.java</li><br>
<li>javacheck -cp $ADDRESS -processor org.checkerframework.checker.fenum.FenumChecker Fraction.java</li><br>
<li>javacheck -cp $ADDRESS -processor org.checkerframework.checker.units.UnitsChecker Fraction.java</li><br>

<h5>Kindly note</h5></br>
<ol>
<li>javacheck is aliased as shown in the manual https://checkerframework.org/manual </li>
<li>$ADDRESS = &lt;path-to-commons-lang&gt;/commons-lang/src/main/java</br> </li>
</ol>
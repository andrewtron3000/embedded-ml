Embedded ML
===========

Overview
--------

<p>This project allows functional programmers to compile functional
programs that can be run on small hobbyist class machines. This
compiler was initially developed by the <a
href="http://www.cs.cmu.edu/afs/cs/Web/Groups/pop/pop.html">Principles
of Programming Group</a> at Carnegie Mellon University by the folks
who brought you the <a href="http://www.boundvariable.org">2006 ICFP
programming contest</a>.  This project contributes two backend code
generators for C and Forth allow the compiler to take on a new role
for functional programmers wanting to target small machines.  The
compiler can only currently target 32-bit machines; however it has
been used to compile several fairly large applications, including:
</p>

<ul>

<li>The UMIX environment that was a critical component in the 2006
ICFP programming contest.  The UMIX environment is several tens of
thousands of lines of ML code.
</li>

<li>Power monitoring software demonstrated within the Scarab rover
developed at the <a href="http://www.frc.ri.cmu.edu">Carnegie Mellon
University Field Robotics Center</a>.  </li>

</ul>

Building The Compiler
---------------------

<p>If you're interested in using the compiler, follow these
instructions to obtain and compile it.  Building the compiler is
hopefully not terribly painful, but it does require that you have the
<a href="http://mlton.org">MLton SML compiler</a> installed, as well
as Subversion and the GNU C Compiler (gcc).</p>

<ol>

<li>
Get MLton installed on your machine.  The instructions for doing this
are provided by the MLton folks.  It is also a package within many
GNU/Linux distributions.
</li>

<li>
Make a working directory on your machine called "mlhacking" or something similar.
</li>

<blockquote>
cd mlhacking
</blockquote>

<li>
Check out (via subversion) <a href="http://tom7.org">tom7</a>'s
sml-lib Standard ML library routines using the following command:
</li>

<blockquote>
svn co https://tom7misc.svn.sourceforge.net/svnroot/tom7misc/trunk/sml-lib sml-lib
</blockquote>

<li>
Clone the embedded-ml repository (this repository).
</li>

<li>
cd embedded-ml
</li>

<li>
Compile the EmbeddedML compiler using the following command:
</li>

<li>
make
</li>

</ol>

<p>
The compiler has been built, and it is named "mlc.exe".  
</p>

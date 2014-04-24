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

<blockquote>
cd embedded-ml
</blockquote>

<li>
Compile the EmbeddedML compiler using the following command:
</li>

<blockquote>
make
</blockquote>

</ol>

<p>
The compiler has been built, and it is named "mlc.exe".  
</p>


Compiling To C
--------------

<li>
First change directories into the tests directory.  There are numerous test files that can be compiled in this directory.
</li>

<blockquote>
cd tests
</blockquote>

<li>
Compile the sandmark.uml file using the EmbeddedML compiler.
</li>

<blockquote>
../mlc.exe -cbackend sandmark.uml
</blockquote>

<li>
This generates C code in the "output" directory, which you will compile and then execute:
</li>

<blockquote>
cd output<br>
make<br>
./a.out<br>
</blockquote>


You should see the following output from the program:

<blockquote>
 == SANDmark 19107 beginning stress test / benchmark.. ==<br>
100. 12345678.09abcdef<br>
99. 6d58165c.2948d58d<br>
98. 0f63b9ed.1d9c4076<br>
97. 8dba0fc0.64af8685<br>
96. 583e02ae.490775c0<br>
95. 0353a77b.2f02685c<br>
94. aa25a8d7.51cb07e5<br>
93. e13149f5.53a9ae5d<br>
92. abbbd460.86cf279c<br>
91. 2c25e8d8.a71883a9<br>
90. dccf7b71.475e0715<br>
89. 49b398a7.f293a13d<br>
88. 9116f443.2d29be37<br>
87. 5c79ba31.71e7e592<br>
86. 19537c73.0797380a<br>
85. f46a7339.fe37b85a<br>
84. 99c71532.729e2864<br>
83. f3455289.b84ced3d<br>
82. c90c81a9.b66fcd61<br>
81. 087e9eef.fc1c13a6<br>
80. e933e2f5.3567082f<br>
79. 25af849e.16290d7b<br>
78. 57af9504.c76e7ded<br>
77. 68cf6c69.6055d00c<br>
76. 8e920fbd.02369722<br>
75. eb06e2de.03c46fda<br>
74. f9c40240.f1290b2a<br>
73. 7f484f97.bc15610b<br>
72. 1dabb00e.61e7b75b<br>
71. dceb40f5.207a75ca<br>
70. c3ed44f5.db631e81<br>
69. b7addb67.90460bf5<br>
68. ae710a90.04b433ef<br>
67. 9ca2d5f0.05d3b631<br>
66. 4f38abe0.4287cc05<br>
65. 10d8691d.a5c934f8<br>
64. 27c68255.52881eaa<br>
63. a0695283.110266b7<br>
62. 336aa5dd.57287a9b<br>
61. b04fe494.d741ddbd<br>
60. 2baf3654.9e33305a<br>
59. fd82095d.683efb19<br>
58. d0bac37f.badff9d7<br>
57. 3be33fcc.d76b127e<br>
56. 7f964f18.8b118ee1<br>
55. 37aeddc8.26a8f840<br>
54. d71d55ff.6994c78f<br>
53. bf175396.f960cc54<br>
52. f6c9d8e1.44b81fd5<br>
51. 6a9b4d86.fe7c66cb<br>
50. 06bceb64.d5106aad<br>
49. 237183b6.49c15b01<br>
48. 4ec10756.6936136f<br>
47. 9d1855a7.1e929fe8<br>
46. a641ede3.36bff422<br>
45. 7bbf5ad4.dd129538<br>
44. 732b385e.39fadce7<br>
43. b7f50285.e7f54c39<br>
42. 42e3754c.da741dc1<br>
41. 5dc42265.928ea0bb<br>
40. 623fb352.3f25bc5b<br>
39. 491f33d9.409bca87<br>
38. f0943bc7.89f512be<br>
37. 80cdbc9d.8ad93517<br>
36. c1a8da99.32d37f3f<br>
35. 91a0b15c.6df2cf4e<br>
34. 50cf7a7a.f0466dc8<br>
33. 02df4c13.14eb615d<br>
32. 2963bf25.d9f06dfe<br>
31. c493d2db.f39ce804<br>
30. 3b6e5a8e.5cf63bd7<br>
29. 4c5c2fbe.8d881c00<br>
28. 9b7354a6.81181438<br>
27. ae0fe8c6.ec436274<br>
26. e786b98d.f5a4111d<br>
25. a7719df1.d989d0b6<br>
24. beb9ebc0.6c56750d<br>
23. edf41fcb.e4cba003<br>
22. 97268c46.713025f1<br>
21. deb087db.1349eb6a<br>
20. fc5221f0.3b4241bf<br>
19. 3fa4370d.8fa16752<br>
18. 044af7de.87b44b11<br>
17. 2e86e437.c4cdbc54<br>
16. fd7cd8aa.63b6ca23<br>
15. 631ceaad.e093a9d5<br>
14. 01ca9732.52962532<br>
13. 86d8bcf5.45bdf474<br>
12. 8d07855b.0224e80f<br>
11. 0f9d2bee.94d86c38<br>
10. 5e6a685d.26597494<br>
9. 24825ea1.72008775<br>
8. 73f9c0b5.1480e7a3<br>
7. a30735ec.a49b5dad<br>
6. a7b6666b.509e5338<br>
5. d0e8236e.8b0e9826<br>
4. 4d20f3ac.a25d05a8<br>
3. 7c7394b2.476c1ee5<br>
2. f3a52453.19cc755d<br>
1. 2c80b43d.5646302f<br>
0. a8d1619e.5540e6cf<br>
SANDmark complete.<br>
</blockquote>
</ol>


Compiling to an Arduino Due Sketch
----------------------------------

<li>
First change directory into the tests subdirectory.
</li>

<blockquote>
cd tests
</blockquote>

<li>
Compile the arduino-test.uml file using the EmbeddedML compiler.
</li>

<blockquote>
../mlc.exe -arduinobackend arduino-test.uml
</blockquote>

<li>
This generates an Arduino compatible ".ino" file in the "output"
directory, which you will need to load into the Arduino IDE where you
can compile and load it to the Arduino Due.
</li>

<li>
The resulting executable will flash the "L" LED on for 500ms and then
off for 500ms.
</li>


Compiling to C for the GumStix
------------------------------

<li>
First change directories into the tests subdirectory.
</li>

<blockquote>
cd tests
</blockquote>

<li>
Compile the sandmark.uml file using the EmbeddedML compiler using the
arm cross compiler for the Gumstix.  Note the assumption is that you
are using the OpenEmbedded Gumstix environment.  Within the Gumstix
OpenEmbedded environment, the cross compilation tools can be found in
gumstix-oe/tmp/cross/bin.</li>

<blockquote>
../mlc.exe -cbackend sandmark.uml -gcctarget arm-angstrom-linux-gnueabi
</blockquote>

<li>
This generates C code in the "output" directory, which you will compile and then execute:
</li>

<blockquote>
cd output<br>
make<br>
./a.out<br>
</blockquote>

The output of this program should be identical to the output shown in the "Compiling to C" section above.

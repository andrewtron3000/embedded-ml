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
cd output
make
./a.out
</blockquote>


You should see the following output from the program:

<blockquote>
 == SANDmark 19107 beginning stress test / benchmark.. ==
100. 12345678.09abcdef
99. 6d58165c.2948d58d
98. 0f63b9ed.1d9c4076
97. 8dba0fc0.64af8685
96. 583e02ae.490775c0
95. 0353a77b.2f02685c
94. aa25a8d7.51cb07e5
93. e13149f5.53a9ae5d
92. abbbd460.86cf279c
91. 2c25e8d8.a71883a9
90. dccf7b71.475e0715
89. 49b398a7.f293a13d
88. 9116f443.2d29be37
87. 5c79ba31.71e7e592
86. 19537c73.0797380a
85. f46a7339.fe37b85a
84. 99c71532.729e2864
83. f3455289.b84ced3d
82. c90c81a9.b66fcd61
81. 087e9eef.fc1c13a6
80. e933e2f5.3567082f
79. 25af849e.16290d7b
78. 57af9504.c76e7ded
77. 68cf6c69.6055d00c
76. 8e920fbd.02369722
75. eb06e2de.03c46fda
74. f9c40240.f1290b2a
73. 7f484f97.bc15610b
72. 1dabb00e.61e7b75b
71. dceb40f5.207a75ca
70. c3ed44f5.db631e81
69. b7addb67.90460bf5
68. ae710a90.04b433ef
67. 9ca2d5f0.05d3b631
66. 4f38abe0.4287cc05
65. 10d8691d.a5c934f8
64. 27c68255.52881eaa
63. a0695283.110266b7
62. 336aa5dd.57287a9b
61. b04fe494.d741ddbd
60. 2baf3654.9e33305a
59. fd82095d.683efb19
58. d0bac37f.badff9d7
57. 3be33fcc.d76b127e
56. 7f964f18.8b118ee1
55. 37aeddc8.26a8f840
54. d71d55ff.6994c78f
53. bf175396.f960cc54
52. f6c9d8e1.44b81fd5
51. 6a9b4d86.fe7c66cb
50. 06bceb64.d5106aad
49. 237183b6.49c15b01
48. 4ec10756.6936136f
47. 9d1855a7.1e929fe8
46. a641ede3.36bff422
45. 7bbf5ad4.dd129538
44. 732b385e.39fadce7
43. b7f50285.e7f54c39
42. 42e3754c.da741dc1
41. 5dc42265.928ea0bb
40. 623fb352.3f25bc5b
39. 491f33d9.409bca87
38. f0943bc7.89f512be
37. 80cdbc9d.8ad93517
36. c1a8da99.32d37f3f
35. 91a0b15c.6df2cf4e
34. 50cf7a7a.f0466dc8
33. 02df4c13.14eb615d
32. 2963bf25.d9f06dfe
31. c493d2db.f39ce804
30. 3b6e5a8e.5cf63bd7
29. 4c5c2fbe.8d881c00
28. 9b7354a6.81181438
27. ae0fe8c6.ec436274
26. e786b98d.f5a4111d
25. a7719df1.d989d0b6
24. beb9ebc0.6c56750d
23. edf41fcb.e4cba003
22. 97268c46.713025f1
21. deb087db.1349eb6a
20. fc5221f0.3b4241bf
19. 3fa4370d.8fa16752
18. 044af7de.87b44b11
17. 2e86e437.c4cdbc54
16. fd7cd8aa.63b6ca23
15. 631ceaad.e093a9d5
14. 01ca9732.52962532
13. 86d8bcf5.45bdf474
12. 8d07855b.0224e80f
11. 0f9d2bee.94d86c38
10. 5e6a685d.26597494
9. 24825ea1.72008775
8. 73f9c0b5.1480e7a3
7. a30735ec.a49b5dad
6. a7b6666b.509e5338
5. d0e8236e.8b0e9826
4. 4d20f3ac.a25d05a8
3. 7c7394b2.476c1ee5
2. f3a52453.19cc755d
1. 2c80b43d.5646302f
0. a8d1619e.5540e6cf
SANDmark complete.
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
cd output
make
./a.out
</blockquote>

The output of this program should be identical to the output shown in the "Compiling to C" section above.


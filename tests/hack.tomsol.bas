V        REM  +------------------------------------------------+
X        REM  | HACK.BAS      (c) 19100   fr33 v4r14bl3z       |
XV       REM  |                                                |
XX       REM  | Brute-forces passwords on UM vIX.0 systems.    |
XXV      REM  | Compile with Qvickbasic VII.0 or later:        |
XXX      REM  |    /bin/qbasic hack.bas                        |
XXXV     REM  | Then run:                                      |
XL       REM  |   ./hack.exe username                          |
XLV      REM  |                                                |
L        REM  | This program is for educational purposes only! |
LV       REM  +------------------------------------------------+
LX       REM
LXV      IF ARGS() > I THEN GOTO LXXXV
LXX      PRINT "usage: ./hack.exe username"
LXXV     PRINT CHR(X)
LXXX     END
LXXXV    REM
XC       REM  get username from command line
XCV      DIM username AS STRING
C        username = ARG(II)
CV       REM  common words used in passwords
CX       DIM words(C) AS STRING
CXV      words(I) = "abcdefg"
CXX      words(II) = "administrator"
CXXV     words(III) = "airplane"
CXXX     words(IV) = "alphabet"
CXXXV    words(V) = "atmosphere"
CXXXVI   REM *****
CXXXVII  wordS(V) = "xavier"
CXXXVIII REM *****
CXL      words(VI) = "aviator"
CXLV     words(VII) = "awesome"
CL       words(VIII) = "blizzard"
CLV      words(IX) = "blowfish"
CLX      words(X) = "bridge"
CLXV     words(XI) = "cactus"
CLXX     words(XII) = "changeme"
CLXXV    words(XIII) = "chicken"
CLXXX    words(XIV) = "creosote"
CLXXXV   words(XV) = "cyclone"
CXC      words(XVI) = "database"
CXCV     words(XVII) = "december"
CC       words(XVIII) = "dolphin"
CCV      words(XIX) = "dragon"
CCX      words(XX) = "electric"
CCXV     words(XXI) = "elephant"
CCXX     words(XXII) = "ersatz"
CCXXV    words(XXIII) = "flower"
CCXXX    words(XXIV) = "foxtrot"
CCXXXV   words(XXV) = "functional"
CCXL     words(XXVI) = "future"
CCXLV    words(XXVII) = "galaxy"
CCL      words(XXVIII) = "garlic"
CCLV     words(XXIX) = "guitar"
CCLX     words(XXX) = "gymnast"
CCLXV    words(XXXI) = "hamster"
CCLXX    words(XXXII) = "hello"
CCLXXV   words(XXXIII) = "helpme"
CCLXXX   words(XXXIV) = "imbroglio"
CCLXXXV  words(XXXV) = "insane"
CCXC     words(XXXVI) = "ironman"
CCXCV    words(XXXVII) = "january"
CCC      words(XXXVIII) = "joshua"
CCCV     words(XXXIX) = "jupiter"
CCCX     words(XL) = "kernel"
CCCXV    words(XLI) = "kingfish"
CCCXX    words(XLII) = "letter"
CCCXXV   words(XLIII) = "logical"
CCCXXX   words(XLIV) = "magic"
CCCXXXV  words(XLV) = "mantra"
CCCXL    words(XLVI) = "memory"
CCCXLV   words(XLVII) = "metal"
CCCL     words(XLVIII) = "monday"
CCCLV    words(XLIX) = "muffin"
CCCLX    words(L) = "nemesis"
CCCLXV   words(LI) = "nutrition"
CCCLXX   words(LII) = "oatmeal"
CCCLXXV  words(LIII) = "october"
CCCLXXX  words(LIV) = "orange"
CCCLXXXV words(LV) = "packard"
CCCXC    words(LVI) = "paladin"
CCCXCV   words(LVII) = "pass"
CD       words(LVIII) = "password"
CDV      words(LIX) = "penelope"
CDX      words(LX) = "penguin"
CDXV     words(LXI) = "pickle"
CDXX     words(LXII) = "polynomial"
CDXXV    words(LXIII) = "popcorn"
CDXXX    words(LXIV) = "puppet"
CDXXXV   words(LXV) = "qwerty"
CDXL     words(LXVI) = "rascal"
CDXLV    words(LXVII) = "rugby"
CDL      words(LXVIII) = "sailor"
CDLV     words(LXIX) = "snowball"
CDLX     words(LXX) = "spider"
CDLXV    words(LXXI) = "stealth"
CDLXX    words(LXXII) = "swordfish"
CDLXXV   words(LXXIII) = "symbol"
CDLXXX   words(LXXIV) = "symmetry"
CDLXXXV  words(LXXV) = "system"
CDXC     words(LXXVI) = "tattoo"
CDXCV    words(LXXVII) = "tennis"
D        words(LXXVIII) = "thursday"
DV       words(LXXIX) = "tinman"
DX       words(LXXX) = "topography"
DXV      words(LXXXI) = "tractor"
DXX      words(LXXXII) = "trivial"
DXXV     words(LXXXIII) = "turtle"
DXXX     words(LXXXIV) = "unicorn"
DXXXV    words(LXXXV) = "vader"
DXL      words(LXXXVI) = "vampire"
DXLV     words(LXXXVII) = "victor"
DL       words(LXXXVIII) = "violet"
DLV      words(LXXXIX) = "viper"
DLX      words(XC) = "warez"
DLXV     words(XCI) = "warrior"
DLXX     words(XCII) = "welcome"
DLXXV    words(XCIII) = "wisdom"
DLXXX    words(XCIV) = "xanadu"
DLXXXV   words(XCV) = "xavier"
DXC      words(XCVI) = "yellow"
DXCV     words(XCVII) = "zephyr"
DC       words(XCVIII) = "zeppelin"
DCV      words(XCIX) = "zombie"
DCX      words(C) = "zxcvbnm"
DCXV     REM try each password
DCXX     DIM i AS INTEGER
DCXXV    i = I
DCXXX    IF CHECKPASS(username, words(i)) THEN GOTO DCL
DCXXXV   i = i + I
DCXL     IF i > C THEN GOTO DCLXV
DCXLV    GOTO DCXXX
DCL      PRINT "found match!! for user " + username + CHR(X)
DCLV     PRINT "password: " + words(i) + CHR(X)
DCLX     END
DCLXV    PRINT "no simple matches for user " + username + CHR(X)
DCLXX    REM
DCLXXV   REM  it is also very common for passwords to take the form
DCLXXX   REM    dictwordDD
DCLXXXV  REM  where DD is a two-digit decimal number. try these next:
DCXC     i = I
DCXCV    GOTO M
M        REM tom's code
MV       DIM p AS STRING
MX       REM
MXV      DIM da AS INTEGER
MXX      DIM db AS INTEGER
MXXV     REM
MXXX     REM *** loop words
MXXXV    REM .... IF i > C THEN GOTO MM  
MXL      PRINT words(i) + CHR(X)
MXLV     da = XXXXVIII
ML       REM *** loop outer
MLV      db = XXXXVIII
MLX      REM *** loop inner
MLXV     p = words(i) + CHR(da) + CHR(db)
MLXX     IF CHECKPASS(username, p) THEN GOTO MCXX
MLXXV    IF db < LVII THEN db = db + I ELSE GOTO MLXXXV 
MLXXX    GOTO MLX
MLXXXV   IF da < LVII THEN da = da + I ELSE GOTO MXCV
MXC      GOTO ML
MXCV     IF i  < CI   THEN i = i + I ELSE GOTO MM
MC       GOTO MXXX
MCV      REM
MCX       REM
MCXV      REM
MCXX     PRINT "hacked " + username + "! pass: " + p + CHR(X)
MCXXV    END
MCXXX     REM
MCXXXV    REM
MCXL      REM
MCXLV     REM
MM       PRINT "no complex matches found" + CHR(X)

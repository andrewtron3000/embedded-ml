import sys

# this program automatically generates (de)marshalling routines, and
# the record definitions that support them.  The following is a valid
# input file that defines several composite structures to generate
# (de)marshalling functions for.

# the program is called as such:  python marshallgen.py inputfile > auto-code.uml

# subject, int, 2, 0
# continuum, int, 3, 0
# unit, int, 2, 0
# role, int, 1, 0
# dvNum, int, 0, 4
# priority, int, 0, 4
# flow, int, 1, 0
# test, composite, unit, role
# fruits, composite, test, subject, continuum
# subscription_array, array, subject, fruits
# subscription_assert, composite, subject, continuum, unit, role, dvNum, priority, flow

def getType(d, x):
    return d[x][0]

def intGetBytes(d, x):
    return int( d[x][1] )

def intGetBits(d, x):
    return int ( d[x][2] )

def stringGetBytes(d, x):
    return int( d[x][1] )

def compositeGetElements(d, x):
    return d[x][1:]

def arrayGetCountVariable(d, x):
    return d[x][1]

def arrayGetComposite(d, x):
    return d[x][2]

def importRecords(ls):
    ''' ls specifies the packet structures '''
    d = {}
    keys = []
    for l in ls:
        l = l.strip()
        xs = l.split(',')
        xs = map( lambda x: x.strip(), xs )
        name = xs[0]
        d[name] = xs[1:]
        keys.append(name)
    return (d, keys)

class outputEngine:
    def __init__(self, indent=0, dx=4):
        self.indentLevel = indent
        self.dx = dx
        self.s = ''
    def getIndent(self):
        return self.indentLevel
    def increaseIndent(self):
        self.indentLevel += self.dx
    def decreaseIndent(self):
        self.indentLevel -= self.dx
    def add(self, l):
        ls = l.split('\n')
        for l in ls:
            if l != '':
                indent = ' ' * self.indentLevel
                self.s += indent + l + '\n'
    def interleave(self, ls, x):
        t = ''
        for l in ls[0:len(ls)-1]:
            t += l
            t += x
        t += ls[len(ls)-1]
        self.add(t)
    def dump(self):
        r = self.s
        self.s = ''
        return r

def integerFromString(d, name):
    return ('marshall-readnextinteger (s, %d, %d)' % ( intGetBytes(d, name), intGetBits(d, name) ))

def stringFromString(d, name):
    return ('marshall-readnextstring s')
    
def arrayFromString(d, name):
    oe = outputEngine()
    oe.add('let \n')
    oe.increaseIndent()
    oe.add('val (n, s) = %s \n' % integerFromString(d, arrayGetCountVariable(d, name)))
    oe.add('fun f (i, s) = \n')
    oe.increaseIndent()
    oe.add(compositeFromString(d, arrayGetComposite(d, name)))
    oe.decreaseIndent()
    oe.decreaseIndent()
    oe.add('in\n')
    oe.increaseIndent()
    oe.add('array-tabulate-st n f s \n')
    oe.decreaseIndent()
    oe.add('end \n')
    return oe.dump()

def compositeFromString(d, name):
    oe = outputEngine()
    xs = compositeGetElements(d, name)
    oe.add('let\n')
    oe.increaseIndent()
    for i, x in enumerate(xs):
        t = getType(d, x)
        if t == 'int':
            oe.add('val (%s, s) = %s\n' % (x, integerFromString(d, x)))
        elif t == 'string':
            oe.add('val (%s, s) = %s\n' % (x, stringFromString(d, x)))
        elif t == 'composite':
            oe.add('val (%s, s) =\n' % x)
            oe.increaseIndent()
            oe.add(compositeFromString(d, x))
            oe.decreaseIndent()
        elif t == 'array':
            oe.add('val (%s, s) =\n' % x)
            oe.increaseIndent()
            oe.add(arrayFromString(d, x))
            oe.decreaseIndent()
        else:
            sys.stderr.write('type not valid: %s, %s' % (x, type))
    oe.decreaseIndent()
    oe.add('in\n')
    oe.increaseIndent()
    oe.add('(\n')
    oe.increaseIndent()
    oe.add('{\n')
    oe.increaseIndent()
    zs = map( lambda n: '%s = %s' % (n, n), xs )
    oe.interleave(zs, ',\n')
    oe.add('\n')
    oe.decreaseIndent()
    oe.add('},\n')
    oe.add('s\n')
    oe.decreaseIndent()
    oe.add(')\n')
    oe.decreaseIndent()
    oe.add('end\n')
    return oe.dump()

def recordName(name):
    return name + '_type'

def targetRecordName(x, name):
    return '#%s/%s r' % (x, recordName(name))

def integerToString(d, x, name):
    return ('marshall-writenextinteger (s, %d, %d, %s)' % ( intGetBytes(d, x), 
                                                            intGetBits(d, x),
                                                            targetRecordName(x, name) ))

def stringToString(d, x, name):
    return ('marshall-writenextstring (s, %s)' % targetRecordName(x, name))

def arrayToString(d, x, name):
    oe = outputEngine()
    oe.add('let \n')
    oe.increaseIndent()
    oe.add('fun f (r, s) = \n')
    oe.increaseIndent()
    oe.add(compositeToString(d, arrayGetComposite(d, x)))
    oe.decreaseIndent()
    oe.decreaseIndent()
    oe.add('in\n')
    oe.increaseIndent()
    oe.add('array-app-st f (%s) s \n' % targetRecordName(x, name))
    oe.decreaseIndent()
    oe.add('end \n')
    return oe.dump()

def compositeToString(d, name, last=''):
    oe = outputEngine()
    xs = compositeGetElements(d, name)
    oe.add('let\n')
    oe.increaseIndent()
    if last != '':
        oe.add('val r = %s' % last)
    for i, x in enumerate(xs):
        t = getType(d, x)
        if t == 'int':
            oe.add('val s = %s\n' % integerToString(d, x, name))
        elif t == 'string':
            oe.add('val s = %s\n' % stringToString(d, x, name))
        elif t == 'composite':
            oe.add('val s =\n')
            oe.increaseIndent()
            oe.add(compositeToString(d, x, last=targetRecordName(x, name)))
            oe.decreaseIndent()
        elif t == 'array':
            oe.add('val s =\n')
            oe.increaseIndent()
            oe.add(arrayToString(d, x, name))
            oe.decreaseIndent()
        else:
            sys.stderr.write('type not valid: %s, %s' % (x, type))
    oe.decreaseIndent()
    oe.add('in\n')
    oe.increaseIndent()
    oe.add('s')
    oe.decreaseIndent()
    oe.add('end\n')
    return oe.dump()


def createRecordDefinition(d, name):
    oe = outputEngine()
    oe.add("type %s =\n" % recordName(name))
    oe.increaseIndent()
    oe.add("{\n")
    oe.increaseIndent()
    zs = []
    for x in compositeGetElements(d, name):
        t = getType(d, x)
        if t == 'composite':
            zs.append('%s : %s' % (x, recordName(x)))
        elif t == 'array':
            e = arrayGetComposite(d, x)
            zs.append('%s : %s array' % (x, recordName(e))) 
        elif t == 'int':
            zs.append('%s : int' % x)
        elif t == 'string':
            zs.append('%s : string' % x)
        else:
            sys.exit('invalid type in a record: %s' % t)
    oe.interleave(zs, ',\n')
    oe.add('\n')
    oe.decreaseIndent()
    oe.add("}\n")
    oe.decreaseIndent()
    return oe.dump()

def createFromStringFunction(d, name):
    oe = outputEngine()
    oe.add("fun %s_fromstring s = \n" % name)
    oe.increaseIndent()
    oe.add('let \n') 
    oe.increaseIndent()
    oe.add('val s = { readbuffer = s, writebuffer = growarray-new (chr 0), byte_offset = 0, bit_offset = 0 } \n')
    oe.add('val (r, s) =\n')
    s = compositeFromString(d, name)
    oe.add(s)
    oe.decreaseIndent()
    oe.add('in\n')
    oe.increaseIndent()
    oe.add('r\n')
    oe.decreaseIndent()
    oe.add('end\n')
    oe.decreaseIndent()
    return oe.dump()

def createToStringFunction(d, name):
    oe = outputEngine()
    oe.add('fun %s_tostring r = \n' % name)
    oe.increaseIndent()
    oe.add('let \n')
    oe.increaseIndent()
    oe.add('val s = { readbuffer = array(1, chr 0), writebuffer = growarray-new (chr 0), byte_offset = 0, bit_offset = 0 } \n')
    oe.add('val s =\n')
    s = compositeToString(d, name)
    oe.add(s)
    oe.decreaseIndent()
    oe.add('in\n')
    oe.increaseIndent()
    oe.add('growarray-subarray ((#writebuffer/marshall_type s), 0, (#byte_offset/marshall_type s))\n')
    oe.decreaseIndent()
    oe.add('end\n')
    oe.decreaseIndent()
    return oe.dump()

if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit('usage: %s inputfile\n' % sys.argv[0])
    
    f = open(sys.argv[1], 'r')
    ls = f.readlines()
    f.close()

    d, ks = importRecords(ls) 

    oe = outputEngine()
    oe.add('val require-marshall = provide-marshall')
    oe.add('\n')

    composites = [x for x in ks if getType(d, x) == 'composite']

    for c in composites:
        oe.add(createRecordDefinition(d, c))
        oe.add('\n')

    for c in composites:
        oe.add(createFromStringFunction(d, c))
        oe.add('\n')
        oe.add(createToStringFunction(d, c))
        oe.add('\n')

    print oe.dump()

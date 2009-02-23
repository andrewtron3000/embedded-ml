# this function generates a record that represents state, along with state accessor functions.

# ams
# roleID, int
# unitID, int
# continuumID, int

# results in
#
# datatype ams-statetype =
#    roleID of int
#  | unitID of int
#  | continuumID of int
#
# fun ams-update-state tv st =
#    case tv of roleID v => { roleid = v, unitid = #unitid/ams-statetype st, continuumid = #continuumID/ams-statetype st }
#             | unitID v => { roleid = #roleID/ams-statetype st, unitid = v, continuumid = #continuumID/ams-statetype st }
#             | continuumID v => { roleid = #roleID/ams-statetype st, unitid = #unitid/ams-statetype st, continuumid = v }
#
# fun ams-query-state t st =
#    case t of roleID => roleID (#roleid/ams-statetype st)
#            | unitID => unitID (#unitid/ams-statetype st)
#            | continuumID => continuumID (#continuumID/ams-statetype st)


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

def createRecordDefinition(d, name):
    oe = outputEngine()
    oe.add("datatype %s =\n" % recordName(name))
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
    return oe.dump()

if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit('usage: %s inputfile\n' % sys.argv[0])
    
    f = open(sys.argv[1], 'r')
    ls = f.readlines()
    f.close()

    d, ks = importRecords(ls[1:]) 


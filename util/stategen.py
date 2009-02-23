import sys
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

def getType(d, x):
    return d[x][0]

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

def createDatatypeDefinition(d, prefix):
    oe = outputEngine()
    oe.add("datatype %s-statetype =\n" % prefix)
    oe.increaseIndent()
    zs = []
    for x in d.keys():
        t = getType(d, x)
        if t == 'int':
            zs.append('%s of int' % x)
        elif t == 'string':
            zs.append('%s of string' % x)
        else:
            sys.exit('invalid type in a record: %s' % t)
    oe.interleave(zs, ' |\n')
    oe.add('\n')
    oe.decreaseIndent()
    return oe.dump()

def recordPicker(d, prefix, y):
    oe = outputEngine()
    oe.increaseIndent()
    es = []
    for x in d.keys():
        if x == y:
            es.append('%s = v' % x.lower())
        else:
            es.append('%s = #%s/%s-statetype st' % (x.lower(), x.lower(), prefix))
    oe.interleave(es, ',\n')
    oe.decreaseIndent()
    return oe.dump()

def createUpdateDefinition(d, prefix):
    oe = outputEngine()
    oe.add("fun %s-update-state tv st =\n" % prefix)
    oe.increaseIndent()
    oe.add("case tv of ")
    oe.increaseIndent()
    zs = []
    for x in d.keys():
        s = '%s v => {\n' % x
        s += recordPicker(d, prefix, x)
        s += ' }'
        zs.append(s)
    oe.interleave(zs, '\n| ')
    oe.add('\n')
    oe.decreaseIndent()
    return oe.dump()

if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit('usage: %s inputfile\n' % sys.argv[0])
    
    f = open(sys.argv[1], 'r')
    ls = f.readlines()
    f.close()

    prefix = ls[0].strip()
    d, ks = importRecords(ls[1:]) 
    print createDatatypeDefinition(d, prefix)
    print createUpdateDefinition(d, prefix)

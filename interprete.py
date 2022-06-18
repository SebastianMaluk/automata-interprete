import ply.lex as lex
import ply.yacc as yacc


reserved={
    'mas' : 'SU',
    'menos' : 'RE',
    "por" : "MUL",
    "entre" : "DIV",
    "igual" : "EQUAL",
    "y" : "AND",
    "o" : "OR",
    "no" : "NOT",
    "si" : "IF",
    "entonces" : "THEN",
    "sino" : "ELSE",
    "mientras" : "WHILE",
    "hacer" : "DO",
    "para" : "FOR",
    "de" : "FROM",
    "a" : "TO",
    "hasta" : "UNTIL",
    "es" : "ASIGN",
    "\(" : "LPAREN",
    "\)" : "RPAREN",
    "-" : "UMINUS",
    "cuadrado" : "SQUARE",
    "triangulo" : "TRIANGLE",
    "rombo" : "RHOMBUS",
    "corazon" : "HEART"
}

tokens = [
    'N',
    'ID'
    ] + list(reserved.values())

t_SU = r'mas'
t_RE = r'menos'
t_MUL = r'por'
t_DIV = r'entre'
t_EQUAL = r'igual'
t_AND = r'y'
t_OR = r'o'
t_NOT = r'no'
t_IF = r'si'
t_THEN = r'entonces'
t_ELSE = r'sino'
t_WHILE = r'mientras'
t_DO = r'hacer'
t_FOR = r'para'
t_FROM = r'de'
t_TO = r'a'
t_UNTIL = r'hasta'
t_ASIGN = r'es'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_UMINUS = r'-'
t_SQUARE = r'cuadrado'
t_TRIANGLE = r'triangulo'
t_RHOMBUS = r'rombo'
t_HEART = r'corazon'
t_ignore = ' \t'


def t_N(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')
    return t

def t_error(t):
    print("Invalid token:",t.value[0])
    t.lexer.skip(1)

precedence = (( 'left', 'SU', 'RE' ),
                ( 'left', 'MUL', 'DIV' ),
                ( 'left', 'EQUAL' ),
                ( 'left', 'AND', 'OR' ),
                ( 'right', 'NOT' ),
                ( 'right', 'UMINUS' ),
                ( 'right', 'IF' ),
                ( 'right', 'THEN' ),
                ( 'right', 'ELSE' ),
                ( 'right', 'WHILE' ),
                ( 'right', 'DO' ),
                ( 'right', 'FOR' ),
                ( 'right', 'FROM' ),
                ( 'right', 'TO' ),
                ( 'right', 'UNTIL' ),
                ( 'right', 'ASIGN' ),
                ('left', 'LPAREN', 'RPAREN'))

variables = {}

def p_resultado(t):
    'resultado : s'
    print(t[1])


def p_expr_num(t):
    's : N'
    t[0] = t[1]

def p_expr_id(t):
    's : ID'
    try:
        t[0] = variables[t[1]]
    except LookupError:
        print("Undefined variable '%s'" % t[1])
        t[0] = 0

def p_expr_op(t):
    '''s : s SU s
            | s RE s
            | s MUL s
            | s DIV s'''
    if t[2] == 'mas':
        t[0] = t[1] + t[3]
    elif t[2] == 'menos':
        t[0] = t[1] - t[3]
    elif t[2] == 'por':
        t[0] = t[1] * t[3]
    elif t[2] == 'entre':
        if t[3] == 0:
            print("Can't divide by zero")
            raise ZeroDivisionError("Division by zero")
        t[0] = t[1] / t[3]

def p_expr_equal(t):
    's : s EQUAL s'
    t[0] = t[1] == t[3]

def p_expr_and(t):
    's : s AND s'
    t[0] = t[1] and t[3]

def p_expr_or(t):
    's : s OR s'
    t[0] = t[1] or t[3]

def p_expr_not(t):
    's : NOT s %prec NOT'
    t[0] = not t[2]

def p_expr_uminus(t):
    's : UMINUS s %prec UMINUS'
    t[0] = -t[2]

def p_expr_if(t):
    's : IF s THEN s ELSE s'
    if t[1]:
        t[0] = t[4]
    else:
        t[0] = t[6]

def p_expr_while(t):
    's : WHILE s DO s'
    while t[1]:
        t[0] = t[3]

def p_expr_for(t):
    's : FOR ID FROM s TO s DO s'
    for i in range(t[3], t[5]):
        variables[t[1]] = i
        t[0] = t[6]

def p_expr_until(t):
    's : UNTIL s DO s'
    while not t[1]:
        t[0] = t[3]

def p_expr_asign(t):
    's : ID ASIGN s'
    variables[t[1]] = t[3]
    print(t[1], "=", t[3])

def p_expr_expr(t):
    's : LPAREN s RPAREN'
    t[0] = t[2]

def p_expr_num_expr(t):
    's : LPAREN N RPAREN'
    t[0] = t[2]

def p_expr_id_expr(t):
    's : LPAREN ID RPAREN'
    try:
        t[0] = variables[t[2]]
    except LookupError:
        print("Undefined variable '%s'" % t[2])
        t[0] = 0

def p_error(t):
    print("Syntax error at '%s'" % t.value)


# Draw square with asterisks
def p_draw_square(t):
    's : SQUARE LPAREN N RPAREN'
    for i in range(t[3]):
        for j in range(t[3]):
            print("* ", end="")
        print()
    t[0] = "END"

# Draw pyramid with asterisks
def p_draw_triangle(t):
    's : TRIANGLE LPAREN N RPAREN'
    for i in range(t[3]):
        for j in range(t[3]):
            if j < t[3] - i - 1:
                print(" ", end="")
            else:
                print("* ", end="")
        print()
    t[0] = "END"

# Draw rhombus with asterisks
def p_draw_rhombus(t):
    's : RHOMBUS LPAREN N RPAREN'
    for x in range(1, (t[3]+5) //2 + 1):
        for y in range( (t[3]+5) //2 - x):
            print("  ", end = "")
        for z in range( (x*2)-1 ):
            print(" *", end = "")
        print()

    for x in range( (t[3]+5)// 2 + 1, t[3] + 5):
        for y in range(x - (t[3]+5) //2):
            print("  ", end = "")
        for z in range( (t[3]+5 - x) *2 - 1):
            print(" *", end = "")
        print()
    t[0] = "END"

# Draw heart with asterisks
def p_draw_heart(t):
    's : HEART LPAREN N RPAREN'
    for i in range (t[3]):
        for j in range (t[3]-i-1):
            print(" ", end="")
        for j in range (i+1):
            print("* ", end="")
        for j in range (2* (t[3]-i-1)):
            print(" ",end ="")
        for j in range (i+1):
            print("* ", end="")
        print()

    for i in range(2*t[3],0, -1):
        for j in range (2*t[3]-i):
            print(" ",end="")
        for j in range (i,0, -1):
            print("* ",end="")
        print()
    t[0] = "END"


lexer = lex.lex()
parser = yacc.yacc()

print()
print("Test: Operations")
res = parser.parse("1 mas 2 por 3")
res = parser.parse("1 mas 2 por  3 menos 4")
res = parser.parse("1 mas 2 por 3 menos 4 entre 5")

print()
print("Test: Logic")
res = parser.parse("1 y 2")
res = parser.parse("1 o 0")

print()
print("Test: Equal")
res = parser.parse("1 igual 1")
res = parser.parse("1 igual 2")

# print()
# print("Test: If")
# res = parser.parse("x es (1 igual 1)")
# res = parser.parse("si (x) entonces cuadrado(5) sino cuadrado(10)")
# print()
# print("Test: While")
# res = parser.parse("mientras 2 hacer 3")
# print()
# print("Test: For")
# res = parser.parse("1 para ID de 1 a 2 hacer 3")
# print()
# print("Test: Until")
# res = parser.parse("1 hasta 2 hacer 3")

print()
print("Test: Draw")
res = parser.parse("cuadrado(5)")
res = parser.parse("triangulo(5)")
res = parser.parse("rombo(5)")
res = parser.parse("corazon(5)")


# while True:
#     try:
#         data = input()
#     except EOFError:
#         break
#     parser.parse(data)

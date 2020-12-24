grid = [["0",  0,  3,  0,"1","0"],
        [  0,  2,  0,  0,  0,  0],
        [  0,  0,"0","0",  0,  5],
        [  6,  0,"0","0",  0,  0],
        [  0,  6,  0,  3,  0,  0],
        ["0","0",  0,  0,  0,"0"]]

'''
grid = [[  3,  4,"0",  0,  0,"0"],
        [  0,  0,"1",  6,  0,"0"],
        [  0,  0,  0,  0,  0,"0"],
        ["0",  0,  0,  0,  1,  0],
        ["0",  0,  3,"0",  0,  0],
        ["0",  0,  0,"0",  0,  0]]
'''
'''
grid = [["0",  0,  0,"1","0","0"],
        ["0",  0,  0,  0,  5,  1],
        ["0",  0,  1,  0,  0,  0],
        [  4,  0,  0,  0,  0,"0"],
        [  0,  6,  5,  0,  0,"0"],
        ["0","0","0",  0,  1,"4"]]
'''
var = ""

# acha menor != 0
def menor(vet):
    menor = 7 
    for e in vet:
        if e!=0 and e<menor:
            menor = e
    return menor


def possible(x,y,n):
    ##
    #print("numero tentado em [" + str(x+1)+ ", "+ str(y+1)+"]:" + str(n))
    #wait = input()
    #var = wait
    ##
    global grid
    for i in range(0, 6):
        var1 = grid[x][i]
        var2 = grid[i][y]
        if type(var1) == str:
            if n == int(var1):
                ##
                #print("é impossivel (preto na linha com mesmo valor)")
                #wait = input()
                ##
                return False
        if type(var2) == str:
            if n == int(var2):
                ##
                #print("é impossivel (preto na COLUNA com mesmo valor)")
                #wait = input()
                ##
                return False
    
    # DONE

    vec_north_south = []
    vec_west_east = []

    # acha os vetores que compõem o intervalo analisado 
    cont = 0
    found_reference = False
    while(cont < 6):
        var = grid[cont][y]
        if(type(var)==int):
            vec_north_south.append(var)
            if(cont == x):
                found_reference = True
        else:
            if(found_reference):
                break
            else:
                vec_north_south = []
        cont+=1
    cont = 0
    found_reference = False
    while(cont < 6):
        var = grid[x][cont]
        if(type(var)==int):
            vec_west_east.append(var)
            if(cont == y):
                found_reference = True
        else:
            if(found_reference):
                break
            else:
                vec_west_east = []
        cont+=1
    
    # DONE


    #print(vec_west_east)
    #print(vec_north_south)
    ##
    #print("vetores encontrados^")
    #wait = input()
    #var = wait
    ##
    # acho intervalo possivel de n
    min_element_we = 1
    max_element_we = 6
    min_element_ns = 1
    max_element_ns = 6
    if(vec_north_south != []):
        max_ns = max(vec_north_south)
        min_ns = menor(vec_north_south)
        len_ns = len(vec_north_south)
        if(max_ns != 0):
            # acho se o vetor só tem um elemento != de 0
            min_element_ns = max_ns - len_ns + 1
            max_element_ns = min_ns + len_ns - 1
            if(max_element_ns>6):
                max_element_ns = 6
            if(min_element_ns<1):
                min_element_ns = 1

    if(vec_west_east != []):
        max_we = max(vec_west_east)
        min_we = menor(vec_west_east)
        len_we = len(vec_west_east)
        if(max_we != 0):
            min_element_we = max_we - len_we + 1
            max_element_we = min_we + len_we - 1
            if(max_element_we>6):
               max_element_we = 6
            if(min_element_we<1):
                min_element_we = 1

    # DONE

    ##
    #print("intervalo possivel:")
    #print("oeste-leste: ["+str(min_element_we)+", "+str(max_element_we)+"]")
    #print("norte-sul:   ["+str(min_element_ns)+", "+str(max_element_ns)+"]")
    #wait = input()
    #var = wait
    ##
    # se o elemento está no intervalo
    if(n >= min_element_ns and n <= max_element_ns and n >= min_element_we and n <= max_element_we):
        # se o elemento não é igual a um elemento ja presente na sequencia
        for i in vec_north_south:
            if(n == i):
                ##
                #print("não é possivel (igual a elemento coluna)")
                #wait = input()
                ## 
                return False
        for i in vec_west_east:
            if(n == i):
                ##
                #print("não é possivel (igual a elemento linha)")
                #wait = input()
                ##    
                return False
        ##
        #print("é possivel")
        #wait = input()
        ##
        return True
    ##
    #print("não é possivel (fora do range)")
    #wait = input()
    ##

    return False

    # DONE
    
def solve():
    #
    #print("solve")
    #wait = input()
    #
    i = 0
    if(iterator_one(i)):
        #print("bruh")
        #print(grid)
        #wait = input()
        return True
    wait = input()
    return False

def iterator_one(i):
    ##
    #print("iterator-one")
    #wait = input()
    ##
    b = iterator_two(i, 0)
    #print(b)
    if(b):
        #print("bruh 1")
        return True

    i += 1
    if (i<6):
        iterator_one(i)
    else:
        # false significa que nao deu merda
        print(grid)
        wait = input()
        return False
    

def iterator_two(i, j):
    ##
    #print("iterator-two")
    #wait = input()
    ##
    if (type(grid[i][j]) == int):
        if (grid[i][j] == 0):
            b = iterator_possibility(i, j, 1)
            if(b):
                #print("big bruh")
                return True
    if (j < 5):
        b = iterator_two(i, j+1)
        if (b):
            #deu ruim, testei tudo e quero retornar
            return True
        else:
            return False
    else:
        ##
        #print("end of line")
        #wait = input()
        ##
        return False


def iterator_possibility(i, j, n):
    
    #print("iterator-possibility")
    #wait = input()
    
    b = possible(i, j, n)
    if (b):
        grid[i][j] = n
        ##
        #for e in grid:
        #    print(e)
        #print()
        ##
        bo = solve()
        ##
        #print("solve retornou")
        #print(bo)
        #wait = input()
        ##
        grid[i][j] = 0
    elif (not b and n == 6):
        #testei totas as possibilidades e não deu
        return True
    if(n<6):
        b = iterator_possibility(i, j, n+1)
        if b:
            return True
    elif(bo == False and n==6):
        # falhou e ultimo testado foi 6, logo epic fail
        return True
    return False

def main():

    solve()
    #print(grid)

if __name__ == "__main__":
    main()


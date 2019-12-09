
# Comments are made with the # operator
"""Multi-line comments are 
enclosed by triple quotes"""


###################################

## Simple Python operations

#Variables
my_variable = 10
my_init = 7
my_float = 1.23

#Booleans
my_bool = True

# Reassign
my_int =  7
my_int = 3
print my_int

#Whitespace
def spam():
  eggs = 12   #Make sure that there is an indent of everything after def
  return eggs   #Return is the end of the def structure and last indented item
  
print spam()

#Math
addition = 72 + 23
subtraction = 108 - 204
multiplication = 108 * 0.5
division = 108 / 9

print addition
print subtraction
print multiplication
print division

#Combine math with other data types (i.e. booleans) and commands to create programs

eight = 2 ** 3   #2^3 = 8

#modulo - returns the remainder from a division
spam = 3 % 2   #Returns 1

################################################################################

####################
## Example 1
####################

#Code for example for previous material
monty = True
python = 1.234
monty_python = python ** 2

####################
## Example 2
####################

#Tip calculator
meal = 44.50
tax = 6.75/100
tip = 0.15

meal = meal + (meal*tax)
total = meal + (meal*tip)

print("%.2f" % total)	#Prints to console the value of total with two numbers after decimal

################################################################################


# Strings

name = "Ryan"
age = "19"
food = 'cheese'
brian = "Hello life!"

#Use escape backslashes or double quotation marks instead of apostrophe to account for internal apostrophes

'This isn\'t flying, this is falling with style'
"This isn't flying, this is falling with style"

#indices
c = "cats"[0]
n = "Ryan"[3]

#String methods let you perform specific tasks for strings
#methods using dot notation only work with strings while len() and str() can
#work on other data types
parrot = "Norwegian Blue"
len(parrot)
parrot.lower()
parrot.upper()

str(2)

#Printing
print "Monty Python"
the_machine_goes = "Ping!"
print the_machine_goes

#String concatenation
print "Life " + "of " + "Brian"
print "The value of pi is around " + str(3.14)

#String formatting with % (when you want to print a variable with a string)
name = "Mike"
print "Hello %s" % (name)

name = raw_input("What is your name?")
quest = raw_input("What is your quest?")
color = raw_input("What is your favorite color?")
print "Ah, so your name is %s, your quest is %s, " \
"and your favorite color is %s." % (name, quest, color)


################################################################################
#Date and Time

from datetime import datetime
now = datetime.now()
print now
print now.year
print now.month
print now.day

print '%s/%s/%s' % (now.month, now.day, now.year)
print '%s:%s:%s' % (now.hour, now.minute, now.second)
print '%s/%s/%s %s:%s:%s' % (now.month, now.day, now.year, now.hour, now.minute, now.second)


################################################################################
#Conditionals and Control Flow

########################
#Example
def clinic():
    print "You've just entered the clinic!"
    print "Do you take the door on the left or the right?"
    answer = raw_input("Type left or right and hit 'Enter'.").lower()
    if answer == "left" or answer == "l":
        print "This is the Verbal Abuse Room, you heap of parrot droppings!"
    elif answer == "right" or answer == "r":
        print "Of course this is the Argument Room, I've told you that already!"
    else:
        print "You didn't pick left or right! Try again."
        clinic()

clinic()
########################

#Boolean operators (and, or, not)
#and
1<2 and 2<3		#True
1<2 and 2>3		#False

#or
1<2 and 2>3		#True
1>2 and 2>3		#False

#not
not False		#True
not 41>40		#False

#not evaluated first, and evaluated second, or evaluated last
False or not True and True		#False
False and not True or True		#True
True and not (False or False)		#True
not not True or False and not True		#True
False or not (True and True)		#False

#Conditional Statement Syntax

# Will the above print statement print to the console?
# Set response to 'Y' if you think so, and 'N' if you think not.
response = 'Y'
answer = "Left"
if answer == "Left":
    print "This is the Verbal Abuse Room, you heap of parrot droppings!"

#elif example	
if 8 > 9:
    print "I don't get printed!"
elif 8 < 9:
    print "I get printed!"
else:
    print "I also don't get printed!"

####################
## Example
####################
#Writing a pig latin translator
print 'Welcome to the Pig Latin Translator!'

pyg = 'ay'
#Ask the user to input a word in English.
original = raw_input("Enter a word:")

#Make sure the user entered a valid word.
if len(original) > 0 and original.isalpha():
  #Convert the word from English to Pig Latin.
  word = original.lower()
  first = word[0]
  new_word = word + first + pyg
  new_word = new_word[1:len(new_word)]
  #Display the translation result.
  print new_word
else:
  print empty

################################################################################
# Functions

def power(base, exponent):  
    result = base**exponent
    print "%d to the power of %d is %d." % (base, exponent, result)

power(37,4)  


#Example (function imports)
from math import sqrt

# Built-in functions
print type(42)		#'int'
print type(4.2)		#'float'
print type('spam')		#'str'

#Overview Example
def shut_down(s):
    if s=='yes':
        return "Shutting down"
    elif s=='no':
        return "Shutdown aborted"
    else:
        return "Sorry"
		
#Example function looking at distance of a value from zero
def distance_from_zero(input):
    if type(input)==int or type(input)==float:
        return abs(input)
    else:
        return "Nope"

#Example - Taking a Vacation
def hotel_cost(nights):
    
    return 140*nights

def plane_ride_cost(city):
    if city=='Charlotte':
        return 183
    elif city == 'Tampa':
        return 220
    elif city == 'Pittsburgh':
        return 222
    elif city == 'Los Angeles':
        return 475

def rental_car_cost(days):
    cost = 40*days
    if days>=7:
        cost -= 50
    elif days>=3:
        cost -= 20
    return cost

def trip_cost(city, days, spending_money):
    return rental_car_cost(days)+hotel_cost(days)+plane_ride_cost(city)+spending_money
    
print trip_cost('Los Angeles', 5, 600)



################################################################################
# Lists

#Arrivals and list length
numbers = [5, 6, 7, 8]

print "Adding the numbers at indices 0 and 2..."
print numbers[0] + numbers[2]
print "Adding the numbers at indices 1 and 3..."
print numbers[1] + numbers[3]

#Appending to lists
suitcase = [] 
suitcase.append("sunglasses")

# Your code here!
suitcase.append('a1')
suitcase.append('a2')
suitcase.append('a3')

list_length = len(suitcase) # Set this to the length of suitcase

print "There are %d items in the suitcase." % (list_length)
print suitcase


#When slicing a list the indices start at the index before the colon and continue up to but not including the index after the colon
animals = "catdogfrog"
cat  = animals[:3]   # The first three characters of animals
dog  = animals[3:6]              # The fourth through sixth characters
frog = animals[6:]               # From the seventh character to the end

#List order
animals = ["aardvark", "badger", "duck", "emu", "fennec fox"]
duck_index = animals.index('duck')   # Use index() to find "duck"

# Your code here!
animals.insert(duck_index, 'cobra')

print animals # Observe what prints after the insert operation

##################
# For loops
#A variable name follows the for keyword; it will be assigned the value of each list item in turn.
#Appending and sorting for loop example
start_list = [5, 3, 1, 2, 4]
square_list = []

# Your code here!
for num in start_list:
    square_list.append(num**2)

square_list.sort()

print square_list

# Dictionaries
#Similar to a list, but you can access values by looking up a key instead of an index
# Assigning a dictionary with three key-value pairs to residents:
residents = {'Puffin' : 104, 'Sloth' : 105, 'Burmese Python' : 106}

print residents['Puffin'] # Prints Puffin's room number

# Your code here!
print residents['Sloth']
print residents['Burmese Python']

##Adding new dictionary entries
menu = {} # Empty dictionary
menu['Chicken Alfredo'] = 14.50 # Adding new key-value pair
print menu['Chicken Alfredo']

# Your code here: Add some dish-price pairs to menu!
menu['Spam']=2.50
menu['Spaghetti']=11.00
menu['Risotto']=9.75

print "There are " + str(len(menu)) + " items on the menu."
print menu

##Example:
inventory = {
    'gold' : 500,
    'pouch' : ['flint', 'twine', 'gemstone'], # Assigned a new list to 'pouch' key
    'backpack' : ['xylophone','dagger', 'bedroll','bread loaf']
}

# Adding a key 'burlap bag' and assigning a list to it
inventory['burlap bag'] = ['apple', 'small ruby', 'three-toed sloth']

# Sorting the list found under the key 'pouch'
inventory['pouch'].sort() 

# Your code here
inventory['pocket']=['seashell', 'strange berry', 'lint']

inventory['backpack'].sort()
inventory['backpack'].remove('dagger')
inventory['gold'] = inventory['gold'] + 50

##################
#Example
##################
webster = {
	"Aardvark" : "A star of a popular children's cartoon show.",
    "Baa" : "The sound a goat makes.",
    "Carpet": "Goes on the floor.",
    "Dab": "A small amount."
}

# Add your code below!
for ind in webster:
    print webster[ind]
	
	
######################################
########## Supermarket Example #######
######################################

shopping_list = ["banana", "orange", "apple"]

stock = {
    "banana": 6,
    "apple": 0,
    "orange": 32,
    "pear": 15
}
    
prices = {
    "banana": 4,
    "apple": 2,
    "orange": 1.5,
    "pear": 3
}

# Write your code below!

def compute_bill(food):
    total = 0
    for item in food:
        if stock[item]>0:
            total = total + prices[item]
            stock[item] = stock[item]-1
        
    return total
	
	
	
###################################################################
###################################################################
###################################################################
###################################################################
###################################################################
###################################################################

#################################
######## Example Exercise #######
##### Gradebook for students ####
#################################

#NOTE: The \ character is a continuation character. The following line is considered a continuation of the current line.
lloyd = {
    "name": "Lloyd",
    "homework": [90.0, 97.0, 75.0, 92.0],
    "quizzes": [88.0, 40.0, 94.0],
    "tests": [75.0, 90.0]
}
alice = {
    "name": "Alice",
    "homework": [100.0, 92.0, 98.0, 100.0],
    "quizzes": [82.0, 83.0, 91.0],
    "tests": [89.0, 97.0]
}
tyler = {
    "name": "Tyler",
    "homework": [0.0, 87.0, 75.0, 22.0],
    "quizzes": [0.0, 75.0, 78.0],
    "tests": [100.0, 100.0]
}

# Add your function below!
def average(numbers):
    total = sum(numbers)
    total = float(total)
    avg = total / len(numbers)
    return avg

def get_average(student):
    homework = average(student['homework'])
    quizzes = average(student['quizzes'])
    tests = average(student['tests'])
    
    sum = 0.10*homework + 0.30*quizzes + 0.60*tests
    return sum

def get_letter_grade(score):
    if score>=90:
        return "A"
    elif score>=80:
        return "B"
    elif score >=70:
        return "C"
    elif score >= 60:
        return "D"
    else:
        return "F"

print get_letter_grade(get_average(lloyd))

def get_class_average(students):
    results = []
    for student in students:
        results.append(get_average(student))
    return average(results)

students = [lloyd, alice, tyler]
classAvg = get_class_average(students)
print classAvg
print get_letter_grade(classAvg)


##Removing items from a list
n = [1, 3, 5]
n.pop(1) 	#removes item at index and returns it to you
n = [1, 3, 5] 	
n.remove(1) 	#removes actual item listed		
n = [1, 3, 5]
del(n[1]) 	#removes the item at index

#Range function is a shortcut for generating a list (you can use ranges in all the same places you can use lists)
range(6) # => [0,1,2,3,4,5]
range(1,6) # => [1,2,3,4,5]
range(1,6,3) # => [1,4]



#################################
######## Example Exercise #######
#####      Battleship        ####
#################################

"""In this project you will build a simplified, 
one-player version of the classic board game Battleship! 
In this version of the game, there will be a single ship 
hidden in a random location on a 5x5 grid. The player 
will have 4 guesses to try to sink the ship."""

from random import randint

board = []

for x in range(5):
    board.append(["O"] * 5)

def print_board(board):
    for row in board:
        print " ".join(row)

print "Let's play Battleship!"
print_board(board)

def random_row(board):
    return randint(0, len(board) - 1)

def random_col(board):
    return randint(0, len(board[0]) - 1)

ship_row = random_row(board)
ship_col = random_col(board)
print ship_row
print ship_col

# Everything from here on should go in your for loop!
# Be sure to indent four spaces!
turn = 0
for turn in range(4):
    # Print (turn + 1) here!
    print "Turn ", turn+1
    turn += 1
    guess_row = int(raw_input("Guess Row:"))
    guess_col = int(raw_input("Guess Col:"))

    if guess_row == ship_row and guess_col == ship_col:
        print "Congratulations! You sunk my battleship!"
        break
    else:
        if (guess_row <= 0 or guess_row > 5) or (guess_col <= 0 or guess_col > 5):
            print "Oops, that's not even in the ocean."
			if turn ==4:
				print "Game Over"
        elif(board[guess_row-1][guess_col-1] == "X"):
            print "You guessed that one already."
			if turn == 4:
				print "Game Over"
        else:
            if turn == 4:
                print "Game Over"
            else:
                print "You missed my battleship!"
                board[guess_row-1][guess_col-1] = "X"
                print_board(board)

				
#########################################################################
##################       Loops        ###################################
#########################################################################

#NOTE: If the loop exits as the result of a break, the else will not be executed.
import random

print "Lucky Numbers! 3 numbers will be generated."
print "If one of them is a '5', you lose!"

count = 0
while count < 3:
    num = random.randint(1, 6)
    print num
    if num == 5:
        print "Sorry, you lose!"
        break
    count += 1
else:
    print "You win!"
	

#Looping over a dictionary - get the key, which you can use to get the value
d = {'x': 9, 'y': 10, 'z': 20}
for key in d:
    if d[key] == 10
        print "This dictionary has the value 10!"


# enumerate works by supplying a corresponding index to each element in the list that you pass it. 
choices = ['pizza', 'pasta', 'salad', 'nachos']

print 'Your choices are:'
for index, item in enumerate(choices):
    print index+1, item
	
#zip will create pairs of elements when passed two lists, and will stop at the end of the shorter list.
list_a = [3, 9, 17, 15, 19]
list_b = [2, 4, 8, 10, 30, 40, 50, 60, 70, 80, 90]

for a, b in zip(list_a, list_b):
    # Add your code here!
    print max(a,b)
	
	
# For/else
fruits = ['banana', 'apple', 'orange', 'pear', 'grape']
#fruits = ['banana', 'apple', 'orange', 'tomato', 'pear', 'grape']

print 'You have...'
for f in fruits:
    if f == 'tomato':
        print 'A tomato is not a fruit!' # (It actually is.)
        break
    print 'A', f
else:
    print 'A fine selection of fruits!'

#Another example:
teams = ['Cubs', 'Nats', 'Cards', 'Royals', 'Pirates']

for team in teams:
    if team=='Royals':
        print "ugh - american League"
        break
    else:
        print "national league"
else:
    print 'All National League!'

	
	
##########################################################################
##########################################################################
##########################################################################

## Sums the digits of a number
def digit_sum(n):
    sum = 0
    for i in range(len(str(n))):
        sum = sum+int(str(n)[i])
    return sum
	
## Calculate factorial
def factorial(x):
    tot = 1
    rem = x
    while rem>=1:
        tot = tot * (rem)
        rem = rem - 1
    return tot

## Determine prime
def is_prime(x):
    div = 0
    if x<=1:
        return Falsedef
    elif x==2:
        return True
    else: 
        for num in range(2,(x)):
            if ((float(x)/num)-int(x/num))==0:
                div=1
                break
            else:
                div=0

    if div==0:
        return True
    else:
        return False


## Scrabble scoring
score = {"a": 1, "c": 3, "b": 3, "e": 1, "d": 2, "g": 2, 
         "f": 4, "i": 1, "h": 4, "k": 5, "j": 8, "m": 3, 
         "l": 1, "o": 1, "n": 1, "q": 10, "p": 3, "s": 1, 
         "r": 1, "u": 1, "t": 1, "w": 4, "v": 4, "y": 4, 
         "x": 8, "z": 10}
         
def scrabble_score(word):
    result = 0
    for w in word:
        result += score[w.lower()]
    return result
	
## Replacing sections of a string
def censor(text, word):
    finalSt = ""
    wordAst = "*"*len(word)
    while text.find(word)>=0:
        findSt = text.find(word)
        findEnd = findSt + len(word)
        finalSt = text[0:findSt] + wordAst + text[findEnd:]
        text = finalSt
    return text

## Removing duplicates
def remove_duplicates(numList):
    finList = []
    for num in numList:
        if num not in finList:
            finList.append(num)
    return finList
	

## Find the median
def median(numList):
    numSorted = sorted(numList)
    if len(numSorted)%2==1:
        if len(numSorted)==1:
            medVal = numSorted[0]
        else:
            medInd = (len(numSorted)/2)
            medVal = float(numSorted[medInd])
    else:
        medOne = (len(numSorted)/2)-1
        medTwo = (len(numSorted)/2)
        medVal = (float(numSorted[medOne])+numSorted[medTwo])/2
    
    return medVal


	
#############################################################################
#############################################################################
#############################################################################

## Classroom exam statistics exercise

grades = [100, 100, 90, 40, 80, 100, 85, 70, 90, 65, 90, 85, 50.5]

def print_grades(grades):
    for grade in grades:
        print grade

def grades_sum(grades):
    total = 0
    for grade in grades: 
        total += grade
    return total
    
def grades_average(grades):
    sum_of_grades = grades_sum(grades)
    average = sum_of_grades / float(len(grades))
    return average

def grades_variance(scores):
    average = grades_average(scores)
    variance = 0
    for score in scores:
        variance += (average-score)**2
    variance = float(variance)/len(scores)
    return variance
    
def grades_std_deviation(variance):
    return variance ** 0.5

variance = grades_variance(grades)

print_grades(grades)
print grades_sum(grades)
print grades_average(grades)
print variance
print grades_std_deviation(variance)


#############################################################################
#############################################################################
#############################################################################

#When iterating through a list, using a trailing comma ensures that we print all on the same line
my_dict = {
    "Name": "David",
    "Age": 28,
    "Birthplace": "Virginia",
    "Marital Status": "Married"
}

for key in my_dict:
    print key, my_dict[key]
	

#List Comprehension
evens_to_50 = [i for i in range(51) if i % 2 == 0]
print evens_to_50

#List Slicing
my_list = range(1, 11)
print my_list[::2]
#reversing a list
my_list = range(1, 11)
backwards = my_list[::-1]

##Anonymous Functions
my_list = range(16)
print filter(lambda x: x % 3 == 0, my_list)

languages = ["HTML", "JavaScript", "Python", "Ruby"]
print filter(lambda x: x=="Python", languages)

#Lambda expressions
garbled = "IXXX aXXmX aXXXnXoXXXXXtXhXeXXXXrX sXXXXeXcXXXrXeXt mXXeXsXXXsXaXXXXXXgXeX!XX"
message =  filter(lambda x: x!='X', garbled)
print message
	

#############################################################################
#############################################################################
#############################################################################
####Python BIT Operations

print 0b1,    #1
print 0b10,   #2
print 0b11,   #3
print 0b100,  #4
print 0b101,  #5
print 0b110,  #6
print 0b111   #7
print "******"
print 0b1 + 0b11
print 0b11 * 0b11

one = 0b1
two = 0b10
three = 0b11
four = 0b100
five = 0b101
six = 0b110
seven = 0b111
eight = 0b1000
nine = 0b1001
ten = 0b1010
eleven = 0b1011
twelve = 0b1100

#When given a string containing a number and the base that number is in, the function int() will return the value of that number converted to base ten.
print int("1",2)
print int("10",2)
print int("111",2)
print int("0b100",2)
print int(bin(5),2)
# Print out the decimal equivalent of the binary 11001001.
print int("11001001",2)

#The bitwise NOT operator (~) just flips all of the bits in a single number
print ~1
print ~2
print ~3
print ~42
print ~123

#Define a function called flip_bit that takes the inputs (number, n).
#Flip the nth bit (with the ones bit being the first bit) and store it in result.
def flip_bit(number, n):
    mask = (0b1<<(n-1))
    result = (number^mask)
    return bin(result)

	
	
#############################################################################
#############################################################################
#############################################################################
####Introduction to Classes

#A class is just a way of organizing and producing objects with similar attributes and methods

#Example:
class Animal(object):
    def __init__(self, name):
        self.name = name
       
zebra = Animal("Jeffrey")
print zebra.name

#Example
class ShoppingCart(object):
    """Creates shopping cart objects
    for users of our fine website."""
    items_in_cart = {}
    def __init__(self, customer_name):
        self.customer_name = customer_name

    def add_item(self, product, price):
        """Add product to the cart."""
        if not product in self.items_in_cart:
            self.items_in_cart[product] = price
            print product + " added."
        else:
            print product + " is already in the cart."

    def remove_item(self, product):
        """Remove product from the cart."""
        if product in self.items_in_cart:
            del self.items_in_cart[product]
            print product + " removed."
        else:
            print product + " is not in the cart."
            
#Create an instance of ShoppingCart called my_cart and initialize it, then use add_tem method to add an item to your cart
my_cart = ShoppingCart('bob')
my_cart.add_item('orange',2.50)


#Class Inheritance
"""Inheritance is the process by which one class takes on the attributes and 
methods of another, and it's used to express an is-a relationship. For example, 
a Panda is a bear, so a Panda class could inherit from a Bear class. However, 
a Toyota is not a Tractor, so it shouldn't inherit from the Tractor class (even 
if they have a lot of attributes and methods in common). Instead, both Toyota 
and Tractor could ultimately inherit from the same Vehicle class."""
class Customer(object):
    """Produces objects that represent customers."""
    def __init__(self, customer_id):
        self.customer_id = customer_id

    def display_cart(self):
        print "I'm a string that stands in for the contents of your shopping cart!"

class ReturningCustomer(Customer):
    """For customers of the repeat variety."""
    def display_order_history(self):
        print "I'm a string that stands in for your order history!"

monty_python = ReturningCustomer("ID: 12345")
monty_python.display_cart()
monty_python.display_order_history()

#Overriding methods through inheritance
class Employee(object):
    """Models real-life employees!"""
    def __init__(self, employee_name):
        self.employee_name = employee_name

    def calculate_wage(self, hours):
        self.hours = hours
        return hours * 20.00

class PartTimeEmployee(Employee):
    def calculate_wage(self, hours):
        self.hours = hours
        return hours * 12.00
		


#You can directly access the attributes or methods of a superclass with Python's built-in super call.
class Employee(object):
    """Models real-life employees!"""
    def __init__(self, employee_name):
        self.employee_name = employee_name

    def calculate_wage(self, hours):
        self.hours = hours
        return hours * 20.00

# Add your code below!
class PartTimeEmployee(Employee):
    def calculate_wage(self, hours):
        self.hours = hours
        return hours * 12.00
    def full_time_wage(self, hours):
        self.hours = hours
        return super(PartTimeEmployee, self).calculate_wage(hours)
        
milton = PartTimeEmployee('milton')

print milton.full_time_wage(10)


#Setting class examples
class Triangle(object):
    number_of_sides = 3
    def __init__(self, angle1, angle2, angle3):
        self.angle1 = angle1
        self.angle2 = angle2
        self.angle3 = angle3
        
    def check_angles(self):
        if (self.angle1+self.angle2+self.angle3)==180:
            return True
        else:
            return False
            
my_triangle = Triangle(90, 30, 60)

print my_triangle.number_of_sides
print my_triangle.check_angles()

class Equilateral(Triangle):
    angle = 60
    def __init__(self):
        self.angle1 = self.angle
        self.angle2 = self.angle
        self.angle3 = self.angle

		
		
#############################################################################
#############################################################################
#############################################################################
####More on Classes (master Examples)

class Car(object):
    condition = "new"
    def __init__(self, model, color, mpg):
        self.model = model
        self.color = color
        self.mpg   = mpg
    
    def display_car(self):
        return "This is a %s %s with %s MPG." % (self.color, self.model, str(self.mpg))
        
    def drive_car(self):
        self.condition = 'used'
        

class ElectricCar(Car):
    def __init__(self, model, color, mpg, battery_type):
        self.model = model
        self.color = color
        self.mpg   = mpg
        self.battery_type = battery_type
        
    def drive_car(self):
        self.condition = 'like new'

#my_car = Car("DeLorean", "silver", 88)

#print my_car.condition
#my_car.drive_car()
#print my_car.condition

my_car = ElectricCar("DeLorean", "silver", 88, 'molten salt')

print my_car.condition
my_car.drive_car()
print my_car.condition



"""Usually, classes are most useful for holding and accessing 
abstract collections of data. One useful class method to override 
is the built-in __repr__() method, which is short for representation; 
by providing a return value in this method, we can tell Python 
how to represent an object of our class (for instance, when using 
a print statement)."""
class Point3D(object):
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z
    
    def __repr__(self):
    #    return "Uh uh"
        return "(%d, %d, %d)" % (self.x, self.y, self.z)
    
my_point = Point3D(1, 2, 3)
print my_point




#############################################################################
#############################################################################
#############################################################################
#### File Input / Output
"""Until now, the Python code you've been writing comes from one source and 
only goes to one place: you type it in at the keyboard and its results are 
displayed in the console. But what if you want to read information from a 
file on your computer, and/or write that information to another file?

This process is called file I/O (the "I/O" stands for "input/output"), and 
Python has a number of built-in functions that handle this for you.

Check out the code in the editor to the right. Note that you now have an extra 
output.txt tab, which is just an empty text file. That's all about to change!"""

##Example:
my_list = [i**2 for i in range(1,11)]
# Generates a list of squares of the numbers 1 - 10

f = open("output.txt", "w")

for item in my_list:
    f.write(str(item) + "\n")

f.close()

#Reading the file
my_file = open('output.txt', 'r')
print my_file.read()
my_file.close()

#Reading files line by line
my_file = open('text.txt', 'r')

print my_file.readline()
print my_file.readline()
print my_file.readline()

my_file.close()

#'with' and 'as' keywords - allow us to open and close file without explicitly calling those commands
with open("text.txt", "w") as textfile:
	textfile.write("Success!")

with open('text.txt','w') as my_file:
    my_file.write("Look I'm doing something in python")
	
	
#Python file objects have a closed attribute which is True when the file is closed and False otherwise.
with open('text.txt','w') as my_file:
    my_file.write("Look I'm doing something in python")
    
if my_file.closed==True:
    my_file.close()

print my_file.closed

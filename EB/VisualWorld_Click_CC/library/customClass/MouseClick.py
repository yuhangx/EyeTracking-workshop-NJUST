##############################################################################
# CustomClass Rules                                                          #
# =================                                                          #
#                                                                            #
#1. All custom classes must inherit sreb.EBObject and the constructor        #
#   must call sreb.EBObject's constructor. If a class starts with _ then it  #
#   is considered internal and will not be treated as a custom class.        #
#                                                                            #
#2. The custom class will only use the default constructor.                  #  
#   ie. a constructor with no parameters.                                    #
#                                                                            #
#3. To add a property provide getter and have an attribute that does not     #
#   starts with an underscore(_) or an upper case letter, and have the       #
#   attribute of a know type. Known types are int,float,str,EBPoint,EBColor, #
#   tuple, and list.                                                         #
#   If an attribute's default value is of type tuple and only has two        #
#   items, the property will be treated as an EBPoint. Similarly, if an      #
#   attribute's default value is a tuple of 3 items the property will be     #
#   treated as an EBColor.  The input type of the setter and the output type #
#   of the getter is expected to be the same as the type of the attribute.   #
#                                                                            #
#4. If only getter is provided, the property will be a readonly property.    # 
#                                                                            #
#6. The custom class may be instanciated during development to obtain        # 
#   class info. Avoid such things as display mode change, remote connections # 
#   in the constructor.                                                      #
#                                                                            # 
#7. Any method in the custom class can be called using the Execute action    #
#   By default, the return type of the method is string unless a doc string  #
#   with the following constraint is available                               #
#	a. The doc string starts with "RETURN:" (case matters)               #
#       b. Following the text "RETURN:" provide a default value of the type  #
#          or the __repr__ value of the class. eg. str for string            #
#8. If a property's setter metthod has default values for it's parameters,   #
#    the property will not accept references or equation.                    #
##############################################################################
import sreb
import sreb.graphics
import sreb.time

class MouseClickCustomClass(sreb.EBObject):
	def __init__(self):
		sreb.EBObject.__init__(self)
		self.iaSet=list()		# list of interest areas in the selection screen.
		self.currentClick = -1;	# used to see which item has been clicked.
	
	
	# Method used to return the currently clicked item
	def getCurrentClick(self):
		return self.currentClick 
		
		
	# Methdod used to determine the current mouse click position in the interest area list;
	# pass in the x and y position of the mouse click		
	def findCurrentIA(self, x, y, width, height):
		"""RETURN: 1 """
		
		self.currentClick = -1;	# First set this to -1
		temp = -1;
			
		# Go through each of the interest area.
		for position in self.iaSet:
			temp += 1;
			if x > position[0] and x < position[0] +  width and  y > position[1] and y < position[1] + height:
				self.currentClick = temp;

		print "current click %d  %d  =>  IA %d"%(int(x), int(y), self.currentClick)
		return self.currentClick
			
			
		
	# Used to reset the trial status
	# need to pass in the location of the images to build an interest area list;
	def reset(self, loc1, loc2, loc3, loc4):
		self.iaSet=list()
		self.iaSet.append(loc1)
		self.iaSet.append(loc2)
		self.iaSet.append(loc3)
		self.iaSet.append(loc4)

		print self.iaSet
		
		# Resets the trial status.
		self.currentClick = -1;
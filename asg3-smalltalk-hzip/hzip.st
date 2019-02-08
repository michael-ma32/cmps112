#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

nl := Character nl.

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Object subclass: ZeroArray 
[
	|theArray|

	ZeroArray class >> new
	[
		self shouldNotImplement.
	]

	ZeroArray class >> new: size 
	[
		|result|
		result := super new.
		result init: size.
		^ result
	]

	init: size 
	[
		theArray := Array new: size.
	]

	size 
	[
 		^ theArray size.
 	]

 	at: index 
	[
		^ theArray at: index + 1.
	]

 	at: index put: value 
	[
 		^ theArray at: index + 1 put: value.
	]

 	incr: index 
	[
 		(self at: index) isNil
 			ifTrue: [ self at: index put: 0].
		self at: index put: (self at: index) + 1.
	]

	keysAndValuesDo: aBlock 
	[
 		(0 to: (self size) - 1) do: 
		[
			:index |
 		 	aBlock value: index value: (self at: index).
 		]
 	]
	
]

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

]

Smalltalk arguments size = 0
	ifTrue: [stdout << 'You fucked up' << nl]
	ifFalse: 
	[
		Smalltalk arguments do: 
			[
				:filename |
				[	
					|inFile|
					inFile := FileStream open: filename mode: FileStream read.
					freqtable := ZeroArray new: 256.
                			[inFile atEnd not] whileTrue: 
					[
						|ordChar|
						ordChar := inFile next asInteger.
						freqtable incr: ordChar.
					].
					priority := SortedCollection new.
					freqtable keysAndValuesDo: 
					[
						:key :value |
						(value notNil and: [value > 0]) ifTrue: 
						[
							"stdout << 'freq[' << key asCharacter visible << ']=' << value << nl.]"
							node := Leaf new: key asCharacter count: value.
							priority add: node.
						]
					].
					[priority size > 1] whileTrue:
					[
						|treeNode1 treeNode2 treeNode3 treeNode1Char frequency|
						treeNode1 := priority removeFirst.
						treeNode2 := priority removeFirst.
						treeNode3 := treeNode1 char.
						frequency := treeNode1 count + treeNode2 count.
						treeNode3 := Tree new: treeNode1Char count: frequency left: treeNode1 right: treeNode2
						priority add treeNode3.
					]
					stdout << priority << nl.
					inFile close.
				]
				on: SystemExceptions.FileError do: 
				[
					:signal |
					|errno|
					errno := File errno.
					stdout flush.
					stderr << execname << ': ' << filename << ': ' << signal messageText << nl.
					stderr flush.
				]
			]
	]

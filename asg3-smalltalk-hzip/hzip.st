#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

nl := Character nl.

Character extend 
[
   isPrint 
   [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible 
   [
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

Object subclass: Leaf 
[
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new 
   [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount 
   [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount 
   [
      char := aChar.
      count := aCount.
   ]

   <= other 
   [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream 
   [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream 
   [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string 
   [
      visitor value: char value: string.
   ]
	postorder: bitsout 
	[
		"write the 0 and then the byte"
		|ascii|
		bitsout writeBit: 0.
		ascii := char asciiValue.
		8 to: 1 by: -1 do: 
		[
			:index |
			|curr|
			curr := ascii bitAt: index.
			bitsout writeBit: curr.
		].
	]
]

Leaf subclass: Tree 
[
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
	postorder: bitsout [
		"recursive call post order on the children"
		"writeout a 1 bit"
		left postorder: bitsout.
		right postorder: bitsout.
		bitsout writeBit: 1.
	]
]

Object subclass: BitStream 
[
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

Smalltalk arguments size = 0
	ifTrue: [stdout << 'unimpl' << nl]
	ifFalse: 
	[
		Smalltalk arguments do: 
			[
				:filename |
				[	
					|inFile|
					inFile := FileStream open: filename mode: FileStream read.
					"create freqtable, based off charfreq.st"
					freqtable := ZeroArray new: 256.
                			[inFile atEnd not] whileTrue: 
					[
						|ordChar|
						ordChar := inFile next asInteger.
						freqtable incr: ordChar.
					].
					"create priority queue, based off charfreq.st and priority.st"
					priority := SortedCollection new.
					freqtable keysAndValuesDo: 
					[
						:key :value |
						(value notNil and: [value > 0]) ifTrue: 
						[
							node := Leaf new: key asCharacter count: value.
							priority add: node.
						]
					].
					"loop to remove two smasllest elements from queue and create new tree"
					[priority size > 1] whileTrue:
					[
						|treeNode1 treeNode2 treeNode3 treeNode1Char frequency|
						treeNode1 := priority removeFirst.
						treeNode2 := priority removeFirst.
						treeNode1Char := treeNode1 char.
						frequency := treeNode1 count + treeNode2 count.
						treeNode3 := Tree new: treeNode1Char count: frequency left: treeNode1 right: treeNode2.
						priority add: treeNode3.
					].
					"construct encoding string for each nonzero character and put in encoding table based off tree.st"
					huffmanTree := priority first.
					encodingTable := ZeroArray new: 256.
					huffmanTree inorder: 
					[
						:char :string |
						charInAscii := char asInteger.
						stdout << charInAscii << nl << string << nl.
						encodingTable at: charInAscii put: string.	
					] prefix: ''.
					"create new filestream"
					outFile := FileStream open: 'temp'.
					asciiByteSequences := OutBits new: outFile.
					"post order traversal of huffmanTree"
					huffmanTree postorder: asciiByteSequences.
					"end of tree"
					asciiByteSequences writeBit: 1.

					inFile close.
					outFile close.
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

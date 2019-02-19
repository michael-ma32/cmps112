#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

nl := Character nl.

FileStream extend [
   |bitIndex currentChar|

   nextBit [
      |bit|
      bitIndex isNil ifTrue: [bitIndex := 0].
      bitIndex = 0 ifTrue: [
         bitIndex := 8.
         currentChar := self next.
      ].
      bit := currentChar value bitAt: bitIndex.
      bitIndex := bitIndex - 1.
      ^ bit
   ]

   atBitEnd [
      ^ (bitIndex isNil | (bitIndex = 0)) & self atEnd
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
      ifTrue: 
      [ 
         self at: index put: 0
      ].
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
   isLeaf [ ^ true ]

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

   "post order traversal"
   postorder: bitsout 
   [
      "write the 0 and then the byte"
      bitsout writeBit: 0.
      8 to: 1 by: -1 do: 
      [
         :index |
         |curr|
         curr := char value bitAt: index.
         bitsout writeBit: curr.
      ].
      char asciiValue = 256 ifTrue: [bitsout writeBit: 1.].
   ]
]

Leaf subclass: Tree 
[
   |left right|

   isLeaf [ ^ false ]
   left [ ^ left ]
   right [ ^ right ]

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

((Smalltalk arguments size < 2) | (Smalltalkarguments size > 3))
ifTrue:
[
   stdout << 'invalid number of arguments' << nl.
]
ifFalse: 
[
   Smalltalk arguments: '-d -c -t -u' do:
   [
      :opt :arg |
      (opt = $d) ifTrue:
      [
         stdout << '-d debug option' << nl.
      ].
      ((opt = $t) | (opt = $c)) ifTrue:
      [
         filename1 := Smalltalk getArgv: 2.
         inFile := FileStream open: filename1 mode: FileStream read.
         "create freqtable, based off charfreq.st"
         freqtable := ZeroArray new: 257.
         freqtable at: 256 put: 1.
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
                freqtable at: key put: value.
                node := Leaf new: key asCharacter count: value.
                priority add: node.
             ]
         ].
         "remove two smallest elements from and create new tree"
         [priority size > 1] whileTrue:
         [
            |treeNode1 treeNode2 treeNode3 treeNode1Char frequency|
            treeNode1 := priority removeFirst.
            treeNode2 := priority removeFirst.
            treeNode1Char := treeNode1 char.
            frequency := treeNode1 count + treeNode2 count.
            treeNode3 := Tree new: 
                         treeNode1Char count:
                         frequency left: treeNode1 right: treeNode2.
            priority add: treeNode3.
         ].
         "construct encoding string for each nonzero character"
         huffmanTree := priority first.
         encodingTable := ZeroArray new: 257.
         huffmanTree inorder: 
         [
            :char :string |
            charInAscii := char asInteger.
            encodingTable at: charInAscii put: string.
         ] prefix: ''.
      ].
      (opt = $t) ifTrue:
      [
         "print out encoding table"
         encodingTable keysAndValuesDo:
         [
            :key :value |
            (value notNil and: [value > 0]) ifTrue:
            [
               getFreq := freqtable at: key.
               "if new line"
               key = 10 ifTrue: 
               [
                  stdout << 'x0A    ' 
                         << getFreq << ' ' << value << nl.
               ]
               "else"
               ifFalse: 
               [
               "if EOF"
                  key = 256 ifTrue: 
                  [
                     stdout << 'EOF     ' 
                            << getFreq << ' ' << value << nl. 
                  ]
                  "else print out encoding Table"
                  ifFalse: 
                  [
                     key = 32 ifTrue: 
                     [
                        stdout << 'x20   '
                               << getFreq << ' ' << value << nl.
                     ]
                     ifFalse:
                     [
                        stdout << ' ' << key asCharacter << '      ' 
                                      << getFreq << ' ' << value << nl.
                     ].
                  ].
              ].
            ]
         ].
      ].
      (opt = $c) ifTrue:
      [
         filename2 := Smalltalk getArgv: 3.
         outFile := FileStream open: filename2.
         outBytes := OutBits new: outFile.
         "post order traversal of huffmanTree"
         huffmanTree postorder: outBytes.
         "end of tree"
         outBytes writeBit: 1.
         "reopen input file and write out encoded version of each byte"
         inFile reset.
         [inFile atEnd not] whileTrue:
         [
            |ordChar| 
            ordChar := inFile next asInteger.
            encodingString := encodingTable at: ordChar.
            encodingString do: 
            [
               :currbit |
               currbit asString = '0'
               ifTrue: [outBytes writeBit: 0]
               ifFalse: [outBytes writeBit: 1].
            ].
         ].
         "encoding string for EOF"
         outBytes writeBit: 1.
         outBytes writeBit: 0.
         outBytes writeBit: 0.
         inFile close.
      ].
      (opt = $u) ifTrue:
      [
         "open compressed file"
         filename1 := Smalltalk getArgv: 2.
         inFile := FileStream open: filename1 mode: FileStream read.
         "create stack"
         stack := OrderedCollection new.
         lastBit := false.
         "recreate huffman tree"
         [(inFile atBitEnd not) & (lastBit = false)] whileTrue:
         [
            bit := inFile nextBit.
            byte := 0.
            "if 0, read next 8 bits and reconstruct the byte"
            bit = 0 ifTrue: 
            [
               8 to: 1 by: -1 do: 
               [
                  :index |
                  bit := inFile nextBit.
                  byte := byte bitAt: index put: bit.
               ].
               "if value is 0, read one more bit"
               byte = 0 ifTrue:
               [
                  bit := inFile nextBit.
                  "if bit is 1, add 256 to byte"
                  bit = 1 ifTrue:
                  [
                     byte := byte + 256.
                  ].
               ].
               "push byte onto stack"
               node := Leaf new: byte asCharacter count: 1.
               stack addFirst: node.
            ]
            "if 1, pop 1 and 0 subtrees from stack and create new tree" 
            ifFalse: 
            [
               stack size = 1 ifTrue:
               [
                  lastBit := true.
               ]
               ifFalse:
               [
                  node1 := stack removeFirst.
                  node0 := stack removeFirst.
                  tree := Tree new: (node1 char) count:
                                    1 left: node0 right: node1.
                  stack addFirst: tree.
               ].
            ].
         ].
         decodingTree := stack first.
         root := decodingTree.
         filename2 := Smalltalk getArgv: 3.
         outFileExist := false.
         (filename2 isNil not) ifTrue:
         [
            outFile := FileStream open: filename2.
            outFileExist := true.
         ].
         "start loop from encoded version of bytes"
         [inFile atBitEnd not] whileTrue:
         [
            bit := inFile nextBit.
            bit = 0 ifTrue:
            [
               "if leaf"
               root isLeaf ifTrue:
               [
                  "check if eof"
                  root char asInteger = 256 ifTrue:
                  [
                     ObjectMemory quit.
                  ].
               ]
               "not leaf"
               ifFalse:
               [
                  "set root to left child"
                  root := root left.
                  "if leaf"
                  root isLeaf ifTrue:
                  [
                     "check if eof"
                     root char asInteger = 256 ifTrue:
                     [
                        ObjectMemory quit.
                     ]
                     ifFalse:
                     [
                        "if there is output file"
                        (outFileExist = true) ifTrue:
                        [
                           outFile << root char.
                        ]
                        "else print to stdout"
                        ifFalse:
                        [
                           stdout << root char.
                        ].
                     ].
                     root := decodingTree.
                  ].
               ].
            ]
            ifFalse: 
            [
               root isLeaf ifTrue:
               [
                  root char asInteger = 256 ifTrue:
                  [
                     ObjectMemory quit.
                  ].
               ]
               ifFalse:
               [
                  root := root right.
                  root isLeaf ifTrue:
                  [
                     root char asInteger = 256 ifTrue:
                     [
                        ObjectMemory quit.
                     ]
                     ifFalse:
                     [
                        (outFileExist = true) ifTrue:
                        [
                           outFile << root char.
                        ]
                        ifFalse:
                        [
                           stdout << root char.
                        ].
                     ].
                     root := decodingTree.
                  ].
               ].
            ].
         ].
      ].
   ].
].

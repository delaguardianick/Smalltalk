Class {
	#name : #Poker,
	#superclass : #Object,
	#category : #Assignment1
}

{ #category : #'as yet unclassified' }
Poker >> compareHands: hands [
	"Takes both hands and compares their evaluations/strength based on pattern, rank and suit"
	"scope: class-variables  &  instance-variables"	
			
	| poker p1 p2 evalp1 evalp2 winner highCardp1 highCardp2 topPairp1 topPairp2|
	
	"Splits hands collection into both individual hands which are already normalized"
	poker := Poker new.
	p1 := hands at: 1.
	p2 := hands at: 2.
	
	"Evaluating the ranking of each hand"
	evalp1 := poker evalHand: p1.
	evalp2 := poker evalHand: p2.

	"Compare evaluations and decide winner accordingly"
	(evalp1 > evalp2)
		ifTrue: [ winner := p1 ].
	(evalp1 < evalp2)
		ifTrue: [ winner := p2 ].
		
	"If tied, means they have same patterns, and winner has to be decided over rank or suit of pattern"
	(evalp1 = evalp2)
		ifTrue: [ 
				"Tie breaking based on high card:"
			(evalp1 = 0)
				ifTrue: [ 
					highCardp1 := (poker tiebreak: p1) at: 1.
					highCardp2 := (poker tiebreak: p2) at: 1.
					
					 (highCardp1 > highCardp2)
							ifTrue: [ winner := p1 ].
				    (highCardp1 < highCardp2)
							ifTrue: [ winner := p2 ]
					 ]
				
				"Tiebreaking based on rank:"
				ifFalse: [
					topPairp1 := (poker tiebreak: p1) at: 2.
					topPairp2 := (poker tiebreak: p2) at: 2.
					
					(topPairp1 > topPairp2 )
						ifTrue: [ winner := p1 ].
					(topPairp1 < topPairp2)
						ifTrue: [ winner := p2 ].
						
				"Tiebreaking based on suit because rank is same "
					((poker tiebreak: p1) = (poker tiebreak: p2))
						ifTrue: [ Transcript show: ('Both hands have same pattern AND rank, no suit-based tiebreaking available');cr ]
					]
				].
		
		^winner.
	
		
	
	
	
	
	
	
	
	
	
	
]

{ #category : #'as yet unclassified' }
Poker >> deal: deck [
	"Top level function to evaluate winner of both hands dealed from one deck."
	"poker hands winner"	
			
	| poker hands winner |
	poker := Poker new.
	
	"Calls makeHands: gives hands alternating deck cards and normalizes the cards rank and suit"
	hands := poker makeHands: deck.
	
	"Prints Hands in normalized format"
	Transcript clear.
	Transcript show: ('Hand 1: '); show: (hands at: 1); cr.
	Transcript show: ('Hand 2: '); show: (hands at: 2); cr.
	
	"Evaluates the winner of both hands"
	winner := poker compareHands: hands.
	
	^winner
	
]

{ #category : #'as yet unclassified' }
Poker >> evalHand: normHand [
	"In order to evaluate all of poker's hands patterns, I considered it be simpler to make
	different variations of a hand according to what I'm looking for."
	"normHand is the real reprentation of the hand ie. ('9C' '8H' '9S' '7C' '11S')"	
	"rankHand is the numeric representation of the hand ie. (9 8 9 7 11)"
	"suitHand is the only-suit representation of the hand ie. ($C $H $S $C $S)"
			
	| poker rankHand suitHand allEvals|
	poker := Poker new.
	rankHand := OrderedCollection new.
	suitHand := OrderedCollection new.
	allEvals := OrderedCollection new.
	
	"rankHand :make new collection with only the ranks of a hand to be able to find patterns easier"
	1 to: 5 do: [ :i | 
		(((normHand at: i )asInteger) = 1) "Makes aces rank 14 instead of 1, makes it easier to find straight"
			ifTrue: [ rankHand add: 14 ]
			ifFalse: [rankHand add: (normHand at: i) asInteger ].
		].
	
	"suitRank: Make new OC with only the suits of the hand, for finding flushes"
	normHand do: [ :each | suitHand add: (each at: (each size))  ].
	
	allEvals add: (poker isXofaKind: rankHand). "Returns either 0,2,4,3,7,8"
	allEvals add: (poker isFlush: suitHand). "Returns 0 or 6"
	allEvals add: (poker isStraight: rankHand). "Returns 0 or 5"
	
	"if hand has a straight and a flush, check for royal flush, if not, then evaluate to straight flush"
	((allEvals includes: 5) and: (allEvals includes: 6)) 
		ifTrue: [ allEvals add: (poker isRoyalFlush: rankHand).
			allEvals add: 9 ].

	"Since we only care for the highest pattern, find the max value of allEvals"
  ^allEvals max.
		
	



]

{ #category : #testing }
Poker >> isFlush: suitHand [
	"Given a suitHand, find the hand is a flush
	Compare all elements, if they are not all of the same suit, not a flush
	Evaluates to 6 on the hand pattern hierarchy of poker."
	"scope: boolean to keep track of weather hand is a flush or not. "
			
	| poker bool eval |
	poker := Poker new.
	bool := true.
	eval := 0.
	
	1 to: (suitHand size -1) do: [ :i | 
		((suitHand at: i) ~= (suitHand at: i+1))
			ifTrue: [ bool := false. ] 
		].
	
	bool
		ifTrue: [ eval := 6 ].
		
	^eval.
		
	
	
	
]

{ #category : #testing }
Poker >> isRoyalFlush: rankHand [
	"Since hand is sent to this method if it already a flush, we just have to check
	if the ranks are 10-14. 14 being Ace. If it is a royal flush, evaluate to 10."
	"scope: Only checked if hand is both a straight and a flush"	
			
	| poker royalFlushRanks eval |
	poker = Poker new.
	eval := 0.
	royalFlushRanks := OrderedCollection with: 10 with: 11 with: 12 with: 13 with: 14.
	
	(royalFlushRanks = rankHand)
		ifTrue: [ eval := 10 ].
		
	^eval.
	
]

{ #category : #testing }
Poker >> isStraight: rankHand [
	"Given rankHand, check if all cards are build a straight
	Hand is already sorted so checking for a straight can be made sequentially."
	"Boolean to keep track if condition has been broken."	
			
	| poker bool eval|
	poker := Poker new.
	bool := true.
	eval := 0.
	
	1 to: (rankHand size -1) do: [ :i |
		(((rankHand at: i) + 1) ~= (rankHand at: i+1))
		 ifTrue: [ bool := false ]
		].
		
	bool
		ifTrue: [ eval := 5 ].
		
	^eval.
	

]

{ #category : #testing }
Poker >> isXofaKind: rankHand [
	"My logic here was to have 1 method to find all patterns that include repetition and evaluate them on the pattern hierarchy (1-10) , which are:
	Pair - 2 ; Two Pair - 3 ; 3 of a Kind - 4 ; Full House - 7 ; 4 of a Kind - 8
	
	In order to do this, I used rankHand (only integers) and split it into a duplicates and a unique OCs.
	Those elements repeated would go into duplicate, so only 1 of each would appear in uniques.
	Once accomplished, the sizes of these lists are already indicative of some patterns
	
	ie. Take hand (8 8 9 10 11) -> duplicates = (8); uniques = (8,9,10,11)
	ie. duplicate size = 1; uniques size = 4.
	1 and 4 are always indicative of a pair, but other patterns like (3 of a kind and 2 pair) and (full house and 4 of a kind)
	have the same list sizes, making results ambiguous
	
	Transforming the duplicate list into a set, deleting repetition of elements, 
	makes the sizes unique for each pattern. I have it planned out on paper if you'd like to see it.
	"
	"scope: returns evaluation of hand"	
			
	| uniques duplicates duplicateSet uSize dSetSize eval|
	
	duplicates := OrderedCollection new.
	uniques := OrderedCollection new.
	eval := 0.
	
	"Makes duplicates and uniques lists"
	rankHand do: [ :each |
			(uniques includes: each)
			ifTrue: [ duplicates add: each  ]
			ifFalse: [ uniques add: each ]
		 ].
	
	"Makes duplicates list a set"
	duplicateSet := Set withAll: duplicates.
	
	"Gets duplicateSet and unique's sizes"
	dSetSize := duplicateSet size.
	uSize := uniques size.
	
	"Case statments corresponding the sizes of the lists to a patter. Patterns are corresponded by evaluation from 1-10"
	((dSetSize == 1) and: (uSize == 4)) "a Pair"
		ifTrue: [ eval := 2 ].
	((dSetSize == 1) and: (uSize == 3)) "3 of a kind"
		ifTrue: [ eval := 4 ].
	((dSetSize == 2) and: (uSize == 3)) "Two pairs"
		ifTrue: [ eval := 3 ].
	((dSetSize == 2) and: (uSize == 2)) "Full House"
		ifTrue: [ eval := 7 ].
	((dSetSize == 1) and: (uSize == 2)) "4 of a Kind"
		ifTrue: [ eval := 8 ].
		
	^eval.
	
	
	
]

{ #category : #'as yet unclassified' }
Poker >> makeHands: deck [
	"#makeHands takes the deck of any size and mimics an alternating dealing of cards in the deck"
	"scope: p1 and p2 represent the player hands which are being dealt to
	since smalltalk doesnt allow returning 2 separate objects, I put both arrays inside another array 	for easy exportation, p1 being 	at index 1 and p2 being at index 2"	
			
	| p1 p2 hands poker|
	p1 := OrderedCollection new.
	p2 := OrderedCollection new.
	hands := OrderedCollection new.
	poker := Poker new.
	
	"Deals cards in deck in alternating fashion to hands p1 and p2"
	1 to: (deck size) do: [ :i | 
		i even ifFalse: [ 
			p1 add: (deck at: i)]
		ifTrue: [ 
			p2 add: (deck at: i) ]
		].
	
	"Calls for hand normalization. Passes from being integers 1-52 to: ints 1-13 + a suit"
	p1 := poker normalizeHand: p1.
	p2 := poker normalizeHand: p2.
	
	"Adds both individual Collections p1,p2 to the collection hands."
	hands add: p1; add: p2.
	
	^hands.
]

{ #category : #'as yet unclassified' }
Poker >> normalizeHand: hand [
	"#normalizeHand transform the representations of cards for easier pattern finding in the future"
	"Transforms cards represented as ints(1-52) to strings with (1-13) + corresponding suit initial"
	"normHand is returned with the real representation of the cards."	
			
	| normHand  |
	normHand := OrderedCollection new.
		
	"Normalizes rank across all suits. made into string for later appending of suits 
	ex 30 % 13 = 4. 4 is the final card rank"
	normHand := hand collect: [ :a | 
		(a % 13 = 0)
			ifTrue: [ 13 asString]
			ifFalse: [ (a % 13) asString]
			].
	
	"Now the rank is normalized, but suits are not. In order to see what suit the integer representing
	the card falls under, we must check the chart. This code justs appends the appropiate 
	suit to the rank, making a real and representation of a card, named normHand. "
	1 to: (hand size) do: [ :i | 
			(hand at: i) >= 40
				ifTrue: [
					normHand at: i put: ((normHand at: i ), 'S'). 
					].
				
			 (((hand at: i) >= 27 ) and: ((hand at: i) <= 39)) ""
				ifTrue: [
					normHand at: i put: ((normHand at: i ), 'H') .
					].
				
			 (((hand at: i) >= 14 ) and: ((hand at: i) <= 26)) ""
				ifTrue: [
					normHand at: i put: ((normHand at: i ), 'D').
					].
				
			  (((hand at: i) >= 1 ) and: ((hand at: i) <= 13)) ""
				ifTrue: [
					normHand at: i put: ((normHand at: i ), 'C') .
					].
		].
 
	^normHand.
	






]

{ #category : #'as yet unclassified' }
Poker >> tiebreak: normHand [
	"Used for tiebreaking. Takes normHand and returns highCard and topPair
	highCard is the highest card of a hand without patterns - Used for tiebreaking if no patterns
	topPair is the highest rank of a pair - used for tiebreaking if patterns are the same"
	"scope: class-variables  &  instance-variables"	
			
	|poker duplicates uniques rankHand topPair highCard return|
	poker := Poker new.
	uniques := OrderedCollection new.
	duplicates := OrderedCollection new.
	rankHand := OrderedCollection new.
	return := OrderedCollection new.
	
	"Transform normHand into rankHand"
	1 to: 5 do: [ :i | 
		(((normHand at: i )asInteger) = 1)
			ifTrue: [ rankHand add: 14 ]
			ifFalse: [rankHand add: (normHand at: i) asInteger ]
		].
	
	"Split rankHand into duplicates and uniques"
	rankHand do: [ :each |
			(uniques includes: each)
			ifTrue: [ duplicates add: each  ]
			ifFalse: [ uniques add: each ]
		 ].
	
	"For tiebreaking when no patterns are avaiable"
	highCard := rankHand max.
	
	"For tiebreaking when patterns are the same ie. pair vs pair"
	(duplicates isEmpty)
		ifTrue: [topPair := 0]
		ifFalse: [topPair := duplicates max].
		
	"Add both highCard and topPair to array to be returned."
	return add: highCard; add: topPair.
		
	^return.
	
	
	
	
	
	
	
]

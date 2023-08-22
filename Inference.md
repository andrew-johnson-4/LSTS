### commute is absurd:commute by type
$$\frac{x:X \quad x:\neg X}{\bot}$$

### commute is absurd:commute by term
$$\frac{f:X \to ! \quad x:X \quad f(x)}{\bot}$$

### commute is absurd:type of term (term undefined)
$$\frac{\Gamma \quad typeof(x)}{\bot}$$

### commute is absurd:term of type (type undefined)
$$\frac{\Gamma \quad termof(X)}{\bot}$$

### commute is absurd:term introduction
### commute is absurd:type introduction
### commute is absurd:term definition
### commute is absurd:type definition

### Primary Transitions
orange -> grey = pun is absurd (ambiguity is impossible to resolve)
yellow -> grey = commute is absurd (False has been proven)
orange -> pink = reduction by terms commute
yellow -> pink = reduction by types commute
grey -> red = blame does not diverge, rule #somerulenumber
pink -> red = soft-eval in type unification (strongly normalizing, sound)
grey -> blue = blame diverges, context #stackdump
pink -> blue = hard-eval in type unification (not strongly normalizing, possibly unsound)
red -> orange = α-conversion,η-reduction,... [pun type]
blue -> orange = β-reduction [pun type]
red -> yellow = α-conversion,η-reduction,... [pun term]
blue -> yellow = β-reduction [pun term]

### Secondary Transitions

commute by term (black circle to black square)
commute by type (white circle to white square)
type of term (black circle to white square)
term of type (white circle to black square)
term introduction (black square to black circle)
type introduction (black square to white circle)
term definition (white square to black circle)
type definition (white square to white circle)

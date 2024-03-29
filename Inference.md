### commute is absurd:commute by type (false has been proven)
$$\frac{x:X \quad x:\neg X}{\bot}$$

### commute is absurd:commute by term (unreachable has been reached)
$$\frac{f:X \to ! \quad x:X \quad f(x)}{\bot}$$

### commute is absurd:type of term (term undefined)
$$\frac{\Gamma \quad x}{\bot}$$

### commute is absurd:term of type (type undefined)
$$\frac{\Gamma \quad X}{\bot}$$

### commute is absurd:term introduction (data is recursive)
$$\frac{x \in x}{\bot}$$

### commute is absurd:type introduction (label is recursive)
$$\frac{X \in X}{\bot}$$

### commute is absurd:term definition (datatype is recursive)
$$\frac{x \in let \ x}{\bot}$$

### commute is absurd:type definition (alias is recursive)
$$\frac{X \in type \ X}{\bot}$$

### note: data and types can potentially be recursive but the AST graph cannot
### relaxing this rule _will_ cause inference to diverge

### pun is absurd:commute by type (rejected conjunction)
$$\frac{x:A \quad x:B \quad x:\neg (A+B)}{\bot}$$

### pun is absurd:commute by term (diamond problem)
$$\frac{x:A \quad x:B \quad f:A\to B \quad f:B\to B \quad f(x)}{\bot}$$

### pun is absurd:type of term (cannot determine object or morphism)
$$\frac{let \ x:A \quad let \ x:B\to C}{\bot}$$

### pun is absurd:term of type (cannot determine color)
$$\frac{let \ x:A\to B \quad let \ x:B\to C \quad let \ x:A\to C}{\bot}$$

### pun is absurd:term introduction (value of plural)
$$\frac{let \ x:A \quad let \ x:B \quad concrete \ x}{\bot}$$

### pun is absurd:type introduction (cycle in proof tree)
$$\frac{A\Rightarrow B \quad B\Rightarrow A}{\bot}$$

### pun is absurd:term definition (binding of plural)
$$\frac{let \ x:A \quad let \ x:B \quad let \ y=x}{\bot}$$

### pun is absurd:type definition (circular type alias)
$$\frac{type \ A=B \quad type \ B=A}{\bot}$$

### note: color, object, and morphisms is category sensitive

### uncategorized
$$\frac{f:A \to B \quad x:A \quad f(x)}{f(x):B}$$

$$\frac{x:A \quad x:B}{x:A+B}$$

## MGU

$$\frac{mgu(X,X)}{X}$$

$$\frac{mgu(X,+Y)}{+\forall Z\in Y \ mgu(X,Z)}$$

$$\frac{mgu(+X,Y)}{mgu(Y,X)}$$

$$\frac{mgu(X<..XS>,Y<..YS>)}{mgu(X,Y)<\forall x\in XS,y\in YS. mgu(x,y))>}$$

$$\frac{mgu(A\to B,X\to Y)}{mgu(A,X)\to mgu(B,Y)}$$

### reduction by terms commute:commute by type
### reduction by terms commute:commute by term
### reduction by terms commute:type of term
### reduction by terms commute:term of type
### reduction by terms commute:term introduction
### reduction by terms commute:type introduction
### reduction by terms commute:term definition
### reduction by terms commute:type definition

### reduction by types commute:commute by type
### reduction by types commute:commute by term
### reduction by types commute:type of term
### reduction by types commute:term of type
### reduction by types commute:term introduction
### reduction by types commute:type introduction
### reduction by types commute:term definition
### reduction by types commute:type definition

### blame, does not diverge:commute by type
### blame, does not diverge:commute by term
### blame, does not diverge:type of term
### blame, does not diverge:term of type
### blame, does not diverge:term introduction
### blame, does not diverge:type introduction
### blame, does not diverge:term definition
### blame, does not diverge:type definition

### soft-eval:commute by type
### soft-eval:commute by term
### soft-eval:type of term
### soft-eval:term of type
### soft-eval:term introduction
### soft-eval:type introduction
### soft-eval:term definition
### soft-eval:type definition

### blame, diverges:commute by type
### blame, diverges:commute by term
### blame, diverges:type of term
### blame, diverges:term of type
### blame, diverges:term introduction
### blame, diverges:type introduction
### blame, diverges:term definition
### blame, diverges:type definition

### hard-eval:commute by type
### hard-eval:commute by term
### hard-eval:type of term
### hard-eval:term of type
### hard-eval:term introduction
### hard-eval:type introduction
### hard-eval:term definition
### hard-eval:type definition

### β-reduction:commute by type
### β-reduction:commute by term
### β-reduction:type of term
### β-reduction:term of type
### β-reduction:term introduction
### β-reduction:type introduction
### β-reduction:term definition
### β-reduction:type definition

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

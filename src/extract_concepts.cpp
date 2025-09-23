#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <bitset>
#include <memory>
#include <cstring>
#include <cstdint>

#define MAX_CONS 50000000       // up to 50 million concepts
#define MAX_COLS 20000          // up to 20,000 attributes
#define MAX_ROWS 2000000        // up to 2 million objects
#define MAX_FOR_B 100000000     // 100 million shorts (~200 MB)
#define MAX_FOR_A 600000000     // 600 million ints (~2.4 GB)

class ConceptMiningState {
public:
    // Context matrices
    alignas(32) uint64_t **contextTemp = nullptr;
    alignas(32) uint64_t **context = nullptr;
    
    // Array sizes
    int mArray = 0;
    int nArray = 0;
    
    // Mapping arrays
    std::vector<int> colOriginal;
    std::vector<int> colSup;
    std::vector<int> rowOriginal;
    
    // Problem dimensions
    int n = 0;      // attributes
    int m = 0;      // objects
    
    // Intent storage (linked list form)
    short int* B = nullptr;
    std::vector<short int> sizeBnode;
    std::vector<short int*> startB;
    std::vector<int> nodeParent;
    std::vector<uint64_t> Bparent;
    std::vector<short int> sizeB;
    short int* bptr = nullptr;
    
    // Extent storage (linear form)
    int* A = nullptr;
    std::vector<int*> startA;
    
    // Algorithm state
    int highc = 0;
    int startCol = 0;
    int numcons = 0;
    
    ConceptMiningState() : 
        colOriginal(MAX_COLS),
        colSup(MAX_COLS),
        rowOriginal(MAX_ROWS),
        sizeBnode(MAX_CONS),
        startB(MAX_CONS, nullptr),
        nodeParent(MAX_CONS),
        Bparent(MAX_COLS/64 + 1),
        sizeB(MAX_CONS),
        startA(MAX_CONS, nullptr) {
        reset();
    }
    
    ~ConceptMiningState() {
        cleanup();
    }
    
    void reset() {
        bptr = nullptr;
        highc = 0;
        startCol = 0;
        std::fill(startA.begin(), startA.end(), nullptr);
        std::fill(startB.begin(), startB.end(), nullptr);
        std::fill(sizeB.begin(), sizeB.end(), 0);
        std::fill(sizeBnode.begin(), sizeBnode.end(), 0);
        std::fill(nodeParent.begin(), nodeParent.end(), 0);
        std::fill(Bparent.begin(), Bparent.end(), 0);
    }
    
    void cleanup() {
        if (contextTemp) {
            for (int j = 0; j < n; j++) {
                delete[] contextTemp[j];
            }
            delete[] contextTemp;
            contextTemp = nullptr;
        }
        
        if (context) {
            for (int i = 0; i < m; i++) {
                delete[] context[i];
            }
            delete[] context;
            context = nullptr;
        }
        
        delete[] A;
        A = nullptr;
        
        delete[] B;
        B = nullptr;
    }
};

using namespace Rcpp;

void populateTempContext(ConceptMiningState& state, const Rcpp::LogicalMatrix& input_context, bool is_dis, const Rcpp::NumericVector item_idx);
void transferToContext(ConceptMiningState& state);
void sortColumns(ConceptMiningState& state);
void sortRows(ConceptMiningState& state);
void InClose(ConceptMiningState& state, const int c, const int y, uint64_t *Bparent);

//[[Rcpp::export]] 
Rcpp::List compute_concepts(Rcpp::LogicalMatrix input_context, bool is_dis, Nullable<NumericVector> item_idx_, bool states_as_matrix, bool give_intents = true) {
    ConceptMiningState state;

    // materialize item_idx if provided
    Rcpp::NumericVector item_idx;
    if (item_idx_.isNotNull()) {
        item_idx = item_idx_.get();
    }
    
    populateTempContext(state, input_context, is_dis, item_idx);
    
    for (int i = 0; i < state.n; i++) state.colOriginal[i] = i;
    sortColumns(state);
    
    transferToContext(state);
    
    for (int i = 0; i < state.m; i++) state.rowOriginal[i] = i;
    sortRows(state);
    
    state.startCol = 0;
    while (state.colSup[state.startCol] == 0) state.startCol++;
    
    state.highc = 1;
    state.A = new int[MAX_FOR_A];
    state.B = new short int[MAX_FOR_B];
    state.bptr = state.B;
    
    for (int i = 0; i < state.m; i++) {
        state.A[i] = i;
    }
    state.startA[0] = &state.A[0];
    state.startA[1] = &state.A[state.m];
    
    state.sizeB[0] = 0;
    state.startB[0] = &state.B[0];
    state.nodeParent[0] = -1;
    
    InClose(state, 0, state.startCol, state.Bparent.data());
    
    // number of states is state.highc+1 (same loop bounds as your original)
    int n_states = state.highc + 1;

    // compute number of columns for the full matrix (max of item_idx or state.m)
    int n_cols;
    if (item_idx_.isNotNull() && !is_dis) {
        // item_idx contains 1-based indices coming from R; find the maximum and cast to int
        n_cols = max(item_idx);
    } else {
        n_cols = state.m;
    }

    // prepare output structures
    Rcpp::LogicalMatrix extents_binary;
    if (states_as_matrix) {
        extents_binary = Rcpp::LogicalMatrix(n_states, n_cols);
        // initialize all entries to is_dis (true/false)
        std::fill(extents_binary.begin(), extents_binary.end(), is_dis);
    }

    Rcpp::LogicalMatrix intents_binary;
    if (give_intents) {
        intents_binary = Rcpp::LogicalMatrix(n_states, state.n);
        std::fill(intents_binary.begin(), intents_binary.end(), true);
    }

    Rcpp::List state_names(n_states);

    // Directly build each state's binary row and name string while iterating A
    for (int c = 0; c < n_states; ++c) {
        int sizeA = (c < state.highc) ? (state.startA[c+1] - state.startA[c])
                                      : state.m - (state.startA[c] - state.A);

        // start with default chars depending on is_dis ('1' means present if not disjunctive)
        std::string state_name(n_cols, is_dis ? '1' : '0');

        int* aptr = state.startA[c];
        for (int i = 0; i < sizeA; ++i) {
            int rowOrigIdx = state.rowOriginal[*aptr++]; // original item index (0-based)
            int it_idx;
            if (item_idx_.isNotNull() && !is_dis) {
                // item_idx from R is 1-based; convert to 0-based index
                it_idx = (int) item_idx[rowOrigIdx] - 1;
            } else {
                it_idx = rowOrigIdx;
            }

            if (states_as_matrix) {
                // for disjunctive functions `is_dis` was used as default; flip appropriately
                extents_binary(c, it_idx) = is_dis ? false : true;
            }
            state_name[it_idx] = is_dis ? '0' : '1';
        }

        if (give_intents) {
            // --- Populate intents ---
            int i = c;
            while (i >= 0) {
                short int* bptr = state.startB[i];
                for (int j = 0; j < state.sizeBnode[i]; j++) {
                    intents_binary(c, state.colOriginal[*bptr++]) = false;
                }
                i = state.nodeParent[i];
            }
            if (c == state.highc) {
                // fill last row with all false
                std::fill(intents_binary.row(c).begin(), intents_binary.row(c).end(), false);
            }
        }

        state_names[c] = state_name;
    }

    if (states_as_matrix) {
        if (give_intents) {
            return Rcpp::List::create(Rcpp::Named("Extents") = extents_binary,
                                      Rcpp::Named("Intents") = intents_binary,
                                      Rcpp::Named("StateNames") = state_names);
        } else {
            return Rcpp::List::create(Rcpp::Named("Extents") = extents_binary,
                                      Rcpp::Named("StateNames") = state_names);
        }
    } else {
        if (give_intents) {
            return Rcpp::List::create(Rcpp::Named("Intents") = intents_binary,
                                      Rcpp::Named("StateNames") = state_names);
        } else {
            return Rcpp::List::create(Rcpp::Named("StateNames") = state_names);
        }
    }
}

void populateTempContext(ConceptMiningState& state, const Rcpp::LogicalMatrix& input_context, bool is_dis, const Rcpp::NumericVector item_idx) {
    // how many unique items are there?
	state.m = is_dis ? max(item_idx) : input_context.nrow();
	int original_nrows = input_context.nrow();
    state.n = input_context.ncol();
    
    state.mArray = (state.m-1)/64 + 1;
    state.contextTemp = new uint64_t*[state.n];
    for (int j = 0; j < state.n; j++) {
        state.contextTemp[j] = new uint64_t[state.mArray];
        for(int i = 0; i < state.mArray; i++) {
            state.contextTemp[j][i] = is_dis ? ~0ULL : 0;
        }
        state.colSup[j] = 0;
    }
    for (int i = 0; i < original_nrows; i++) {
		int it_idx = is_dis ? (int) item_idx[i] - 1 : i;
        for (int j = 0; j < state.n; j++) {	
			if (!is_dis) {
				if (input_context(i, j)) {
					state.contextTemp[j][(it_idx >> 6)] |= (1ULL << (it_idx % 64));
					state.colSup[j]++;
				}
			} else {
				if (!input_context(i, j)) {
					// invert here, write 0
					state.contextTemp[j][(it_idx >> 6)] &= ~(1ULL << (it_idx % 64));
					state.colSup[j]++;
				}
			}
        }
    }
}

void transferToContext(ConceptMiningState& state) {
    state.nArray = (state.n-1)/64 + 1;
    state.context = new uint64_t*[state.m];
    
    for (int i = 0; i < state.m; i++) {
        state.context[i] = new uint64_t[state.nArray];
        for(int j = 0; j < state.nArray; j++) state.context[i][j] = 0;
    }
    
    for (int i = 0; i < state.m; i++) {
        for (int j = 0; j < state.n; j++) {
            if (state.contextTemp[j][(i >> 6)] & (1ULL << (i % 64))) {
                state.context[i][j >> 6] |= (1ULL << (j % 64));
            }
        }
    }
    
    for (int j = 0; j < state.n; j++) {
        delete[] state.contextTemp[j];
        state.contextTemp[j] = nullptr;
    }
    delete[] state.contextTemp;
    state.contextTemp = nullptr;
}

bool IsCannonical(ConceptMiningState& state, const int y, const int * endAhighc, const uint64_t Bchild[])
/* y: attribute number, endAhighc: pointer to end of the next extent to be created */
/* Bchild: the current intent in Boolean form (to skip columns when checking cannonocity of any 'new' concept) */
{

	//initialse Bmask
	uint64_t Bmask[MAX_COLS/64 + 1];
	int p;
	for(p = 0; p < y>>6; p++){
		Bmask[p]=~Bchild[p]; //invert 64 bit chunks of current intent
	}
	Bmask[p]= ~Bchild[p] & ((1ULL << (y % 64))-1); //invert last 64 bits up to current attribute

	for(p=0; p <= y>>6; p++){
		int i;
		int * Ahighc = state.startA[state.highc];	//find start of extent
		for(i = endAhighc - Ahighc; i > 0; i--){	 //iterate from number of objects downto zero
			Bmask[p] = Bmask[p] & state.context[*Ahighc][p]; //apply mask to context (testing 64 cells at a time)
			if(!Bmask[p])break;		//if there is nothing still true then stop looking down this 64 columns
			Ahighc++;				//otherwise, next object
		}
		if(i==0) return(false);
	}
	return(true);	//if intersection is not found, it is cannonical
}


void sortColumns(ConceptMiningState& state)
{
	int temp,i,j;
	/* bubble sort column indexes (logical sort) - ascending order of support*/
	for (i = 0 ; i < state.n ; i++){
		for (j = 0 ;j <  state.n-i-1; j++){
			if(state.colSup[j] > state.colSup[j+1]){
				temp = state.colSup[j];
				state.colSup[j] = state.colSup[j+1];
				state.colSup[j+1] = temp;
				temp = state.colOriginal[j];
				state.colOriginal[j] = state.colOriginal[j+1];	//keep track of original columns
				state.colOriginal[j+1] = temp;
			}
		}
	}

	/* rewrite sorted context (physical sort) */
	int tempColNums[MAX_COLS];
	int rank[MAX_COLS];
	for(j = 0; j < state.n; j++){
		tempColNums[j]=state.colOriginal[j]; //use original col nos to index the sort
		rank[state.colOriginal[j]]=j;			//record the ranking of the column
	}
	for(j = 0; j < state.n - 1; j++){
		for(i = 0; i < state.mArray; i++){
			uint64_t temp = state.contextTemp[j][i];
			state.contextTemp[j][i] = state.contextTemp[tempColNums[j]][i];
			state.contextTemp[tempColNums[j]][i] = temp;
		}
		tempColNums[rank[j]]=tempColNums[j];		//make note of where swapped-out col has moved to using its rank
		rank[tempColNums[j]]=rank[j];
	}
}

void sortRows(ConceptMiningState& state)
{

	void quickBent2Hamsort(ConceptMiningState& state, int left,int right,int original[]);

	quickBent2Hamsort(state, 0, state.m-1, state.rowOriginal.data());

	/* rewrite sorted context (physical sort) */
	int* tempRowNums = new int[MAX_ROWS];
	int* rank = new int[MAX_ROWS];

	for(int i = state.m-1; i >= 0; i--){
		tempRowNums[i]=state.rowOriginal[i];	//use original row nos to index the sort
		rank[state.rowOriginal[i]]=i;			//record the ranking of the row
	}

	for(int i = state.m-1; i >= 0; i--){
		for(int j = state.nArray - 1; j >= 0; j--){
			uint64_t temp = state.context[i][j];
			state.context[i][j] = state.context[tempRowNums[i]][j];
			state.context[tempRowNums[i]][j] = temp;
		}
		tempRowNums[rank[i]]=tempRowNums[i];		//make note of where swapped-out row has moved to using its rank
		rank[tempRowNums[i]]=rank[i];
	}
	delete[] tempRowNums;
	delete[] rank;
}


void quickBent2Hamsort(ConceptMiningState& state, int left,int right,int original[])
{
	bool biggerHam(ConceptMiningState& state, int i1, int i2);
	bool eq(ConceptMiningState& state, int i1, int i2);
	void exchange(int original[], int from, int to);
	void insertionSort(ConceptMiningState& state, int left, int right, int original[]);
	int median3(ConceptMiningState& state, int original[], int i, int j, int k);

	if(right < left) return;

	int _n = right - left + 1;

	if(_n <= 8) {
		insertionSort(state, left, right, original);
		return;
	} else if (_n <= 60) {
		int _m = median3(state, original, left, left + _n/2, right);
		exchange(original, _m, left);
	} else {
		  int eps = _n/8;
            int mid = left + _n/2;
            int m1 = median3(state, original, left, left + eps, left + eps + eps);
            int m2 = median3(state, original, mid - eps, mid, mid + eps);
            int m3 = median3(state, original, right - eps - eps, right - eps, right);
            int ninther = median3(state, original, m1, m2, m3);
            exchange(original, ninther, left);
	}


	int i = left;
	int j = right + 1;

	int p = left;
	int q = right + 1;

	int pivotIndex = left;
	int pivotValue =  original[pivotIndex];

	while(true) {

		while(biggerHam(state, original[++i], pivotValue))
			if(i == right)
				break;

		while(biggerHam(state, pivotValue, original[--j]))
			if(j == left)
				break;

		// pointers cross
		if(i == j && eq(state, original[i], pivotValue))
			exchange(original, ++p, i);

		if(i >= j)
			break;

		exchange(original, i, j);

		if(eq(state, original[i], pivotValue))
			exchange(original, ++p, i);
		if(eq(state, original[j], pivotValue))
			exchange(original, --q, j);
	}


	// exhange equal parts on left and right sides to the middle
	i = j + 1;
	for(int k = left; k <= p; k++)
		exchange(original, k, j--);
	for(int k = right; k >= q; k--)
		exchange(original, i++, k);

	quickBent2Hamsort(state, left, j, original);
	quickBent2Hamsort(state, i, right, original);

	}

void insertionSort(ConceptMiningState& state, int left, int right, int original[]) {

	bool biggerHam(ConceptMiningState& state, int i1, int i2);
	void exchange(int original[], int left, int right);

	for(int i = left; i <= right; i++) {
		for( int j = i;  j > left && biggerHam(state, original[j], original[j-1]); j--)
			exchange(original, j, j-1);
	}
}

int median3(ConceptMiningState& state, int original[], int i, int j, int k) {

	bool biggerHam(ConceptMiningState& state, int i1, int i2);

	int med;
	biggerHam(state, original[i], original[j]) ?
               (biggerHam(state, original[j], original[k]) ? med = j : biggerHam(state, original[i], original[k]) ? med = k : med = i) :
               (biggerHam(state, original[k], original[j]) ? med = j : biggerHam(state, original[k], original[i]) ? med = k : med = i);
	return med;
}

bool biggerHam(ConceptMiningState& state, int i1, int i2)
{
	for(int j=state.nArray-1;j>=0;j--){
		if(state.context[i1][j]>state.context[i2][j])return true;
		if(state.context[i2][j]>state.context[i1][j])return false;
	}
	return false;
}

bool eq(ConceptMiningState& state, int i1, int i2)
{
	for(int j=state.nArray-1;j>= 0;j--){
		if(state.context[i1][j] != state.context[i2][j])
			return false;
	}
	return true;
}

void exchange(int original[], int from, int to) {

	int temp = original[from];
	original[from] = original[to];
	original[to] = temp;
}



void InClose(ConceptMiningState& state, const int c, const int y, uint64_t *Bparent)
/* c: concept number, y: attribute number, Bparent: parent intent in Boolean form */
{
	bool IsCannonical(ConceptMiningState& state, const int y,  const int * endAhighc, const uint64_t Bparent[]);
	/* y: attribute number, endAhighc: pointer to end of the next extent to be created */
	/* Bchild: the current intent in Boolean form (to skip columns when checking cannonocity of any 'new' extent) */

	int Bchildren[MAX_COLS];							//the attributes that will spawn new concepts
	int numchildren = 0;								//the number of new concepts spawned from current one
	int Cnums[MAX_COLS];								//the concept no.s of the spawned concepts
	uint64_t Bchild[MAX_COLS/64 + 1];				//the current intent in Boolean form

	
	/*********************** MAIN LOOP *********************************************************
		interate across attribute columns forming column intersetcions with current extent
	********************************************************************************************/
	int sizeAc = state.startA[c+1]-state.startA[c];			//calculate the size of current extent
	//for(int j = y; j >= 0; --j)	{
	for(int j = y; j < state.n; j++) {
		if(!(Bparent[j>>6] & (1ULL << (j % 64)))){
			//inters++;
			//if attribute is not an inherited one
			int * Ac = state.startA[c];						//pointer to start of current extent
			int * aptr = state.startA[state.highc];					//pointer to start of next extent to be created

			/* iterate across objects in current extent to find them in current attribute column */
			for(int i = sizeAc; i > 0; i--){
				if(state.context[*Ac][j>>6] & (1ULL << (j % 64))){//context[*Ac][J] where J is byte J div 8, bit J mod 8
					*aptr = *Ac;						//add object to new extent (intersection)
					aptr++;
				}
				Ac++;									//next object
			}

			int size = aptr - state.startA[state.highc];			//calculate size of intersection

			if(size==0){
				Bparent[j>>6] = Bparent[j>>6] | (1ULL << (j % 64));	//intersection is empty, so the column can be ignored in subsequent levels
			}
			else {
				if(size < sizeAc){
					//test++;
					if(IsCannonical(state, j,aptr,Bparent)){	//if the intersection is a new extent, note the child for later spawning:
						Bchildren[numchildren] = j;			//note where (attribute column) it was found,
						Cnums[numchildren++] = state.highc;		//note the concept number,
						state.nodeParent[state.highc] = c;				//note the parent concept number and
						state.startA[++state.highc] = aptr;				//note the start of the new extent in A.
						/*if(highc == MAX_CONS){
							cout << "ooops";
						}*/
					}
				}
				else {		//size == sizeAc: extent is unchanged
					*state.bptr = j;							//add current attribute to intent
					state.bptr++;
					Bparent[j>>6] = Bparent[j>>6] | (1ULL << (j % 64));		//record that the attribute will be inherited by any child concepts
					state.sizeBnode[c]++;						//increment the number of attributes at this node in the B tree
				}
			}
		}
	}
	/* spawn child concepts from this parent */
	for(int i = numchildren-1; i >= 0 ; i--){
		memcpy(Bchild,Bparent,state.nArray*8);	//set the child attributes to the parent ones (inheritance)
		Bchild[Bchildren[i]>>6] = Bchild[Bchildren[i]>>6] | (1ULL << (Bchildren[i] % 64));
		*state.bptr = Bchildren[i];
		state.startB[Cnums[i]] = state.bptr;	//set the start of the intent in B tree	
		state.bptr++;
		state.sizeBnode[Cnums[i]]++;			
		InClose(state, Cnums[i], Bchildren[i]+1, Bchild);		//close the child concept
	}	
}

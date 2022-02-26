import random
from Bio import SeqIO

# inputs
input_file="./celegans_genomic.fa"
output_file="./celegans_chr1-chr2_rearranged.fa"
chunk_size=5 * (10 ** 5)
#chunk_size=10

## local variables
names=list()
sequences=list()
it=0
misassembled_seq=">toy.fa_rearranged_500kbchunks \n"

def add_N_to_assembly(chunk_size):
    return misassembled_seq + "N"*chunk_size

def add_to_assembly(idx, chunk_size):
    if len(sequences[idx]) >= chunk_size: 
        new_seq = misassembled_seq + sequences[idx][:chunk_size]
        sequences[idx]=sequences[idx][chunk_size:]
    else:
        new_seq = misassembled_seq
    if len(sequences[idx]) < chunk_size: #/ if this chromosome has fewer than chunk_zie basepairs remained, skip it  
        chromosome_set.remove(idx)
    return new_seq

## parse input into lists
fa_seqs = SeqIO.parse(open(input_file),'fasta')
for fa_seq in fa_seqs:
    names.append(fa_seq.id)
    sequences.append(fa_seq.seq)
    it=it+1

###### only if you want to pick 2 chromosomes:
#new_sequences = list()
#new_sequences.append(sequences[0])
#new_sequences.append(sequences[1])
#sequences = new_sequences

chromosome_set = set(range(0,len(sequences))) # set of chromosome with sequences remained to use
last_choice=-1
## randomly split into chunks of chunk_size
while len(chromosome_set) > 1:
    new_choice = random.choice(tuple(chromosome_set))
    while new_choice == last_choice:
        new_choice = random.choice(tuple(chromosome_set))
    misassembled_seq = add_to_assembly(new_choice, chunk_size)
    last_choice = new_choice

#/ len(chromosome_set) == 1 >> one chromosome not included fully.
if last_choice != list(chromosome_set)[0]:
    misassembled_seq = add_to_assembly(list(chromosome_set)[0], chunk_size)

while len(chromosome_set) > 0:
    misassembled_seq = add_N_to_assembly(chunk_size)
    misassembled_seq = add_to_assembly(list(chromosome_set)[0], chunk_size)

f = open(output_file, "w")
f.write(str(misassembled_seq))
f.close()

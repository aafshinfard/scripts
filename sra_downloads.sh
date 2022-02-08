#!/bin/bash

srx_id=$1

sra_ids=$(wget -qO- "http://trace.ncbi.nlm.nih.gov/Traces/sra/sra.cgi?save=efetch&db=sra&rettype=runinfo&term=${srx_id}" | grep ${srx_id} | cut -f1 -d",")

for sra_id in "${sra_ids[@]}"; do
    fastq-dump ${sra_id}
done

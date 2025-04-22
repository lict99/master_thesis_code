#! /bin/bash

# Definition of variables
OUTPUT_DIR="./results/09"
RAW_BFILE="./data/snp/asa_array/raw_data/22B0218D_23B0203E_23P1212B00A_24B0516F"
IMP_BFILE="./data/snp/asa_array/impute_qc_data/final"
IV_FILE="./results/07/iv.txt"
SEX_FILE="./results/08/sex_info.txt"

# Setting up output directory
if [ ! -d $OUTPUT_DIR ]; then
    mkdir -p $OUTPUT_DIR
fi

# Updating sex information in the raw SNP data
plink \
    --bfile $RAW_BFILE \
    --update-sex $SEX_FILE \
    --mind 0.05 \
    --geno 0.05 \
    --make-bed \
    --out "$OUTPUT_DIR/raw_data_with_sex"

# Checking for sex discrepancies
plink \
    --bfile "$OUTPUT_DIR/raw_data_with_sex" \
    --check-sex \
    --out "$OUTPUT_DIR/sexcheck"

# Deleting intermediate files
find $OUTPUT_DIR -type f -name "*raw_data_with_sex*" -delete

# Extraction of samples with sex discrepancies
grep "PROBLEM" "$OUTPUT_DIR/sexcheck.sexcheck" | awk '{print$1,$2}' >"$OUTPUT_DIR/sex_discrepancy.txt"

# Removing samples with sex discrepancies from the imputed SNP data
plink \
    --bfile $IMP_BFILE \
    --remove "$OUTPUT_DIR/sex_discrepancy.txt" \
    --make-bed \
    --out "$OUTPUT_DIR/imp_data_with_sexcheck"

# Performing principal component analysis (PCA)
plink \
    --bfile "$OUTPUT_DIR/imp_data_with_sexcheck" \
    --pca 5 \
    --out "$OUTPUT_DIR/pca5"

# Extracting needed SNPs for the final dataset
plink \
    --bfile "$OUTPUT_DIR/imp_data_with_sexcheck" \
    --extract $IV_FILE \
    --make-bed \
    --out "$OUTPUT_DIR/data_final"

# Deleting intermediate files
find $OUTPUT_DIR -type f -name "*imp_data_with_sexcheck*" -delete

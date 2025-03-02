#! /bin/bash

#
OUTPUT_DIR="./results/09"
RAW_BFILE="./data/snp/asa_array/raw_data/22B0218D_23B0203E_23P1212B00A_24B0516F"
IMP_BFILE="./data/snp/asa_array/impute_qc_data/final"
IV_FILE="./results/07/iv.txt"
SEX_FILE="./results/08/sex_info.txt"

#
if [ ! -d $OUTPUT_DIR ]; then
    mkdir -p $OUTPUT_DIR
fi

#
plink \
    --bfile $RAW_BFILE \
    --update-sex $SEX_FILE \
    --mind 0.05 \
    --geno 0.05 \
    --make-bed \
    --out "$OUTPUT_DIR/raw_data_with_sex"

#
plink \
    --bfile "$OUTPUT_DIR/raw_data_with_sex" \
    --check-sex \
    --out "$OUTPUT_DIR/sexcheck"

find $OUTPUT_DIR -type f -name "*raw_data_with_sex*" -delete

#
grep "PROBLEM" "$OUTPUT_DIR/sexcheck.sexcheck" | awk '{print$1,$2}' >"$OUTPUT_DIR/sex_discrepancy.txt"

#
plink \
    --bfile $IMP_BFILE \
    --remove "$OUTPUT_DIR/sex_discrepancy.txt" \
    --make-bed \
    --out "$OUTPUT_DIR/imp_data_with_sexcheck"

#
plink \
    --bfile "$OUTPUT_DIR/imp_data_with_sexcheck" \
    --pca 5 \
    --out "$OUTPUT_DIR/pca5"

#
plink \
    --bfile "$OUTPUT_DIR/imp_data_with_sexcheck" \
    --extract $IV_FILE \
    --make-bed \
    --out "$OUTPUT_DIR/data_final"

find $OUTPUT_DIR -type f -name "*imp_data_with_sexcheck*" -delete

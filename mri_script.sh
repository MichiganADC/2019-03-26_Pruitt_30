#!/bin/bash

LOCAL_MRI_DIR="${HOME}/MRI/2019-03-26_Pruitt_30/"

echo 'Getting MRI directories on `madcbrain` server.'
ssh -q ldmay@madcbrain.umms.med.umich.edu \
  'cd /nfs/fmri/RAW_nopreprocess/; \
  ls | grep -E "^(bmh|hlp)17umm0[0-9]{4}_[0-9]{5}$" | \
  sort -k5,5gr -t "_"' | \
  cat > "${LOCAL_MRI_DIR}/mri_madcbrain.txt"

cut -d , -f 10 df_u3_ug_ms_cln_mut_2020-04-08.csv | tail -n "$(( $(df_u3_ug_ms_cln_mut_2020-04-08.csv | wc -l) - 1))" | sort | uniq | cat > "${LOCAL_MRI_DIR}/mri_pruitt.txt"

comm -12 "${LOCAL_MRI_DIR}/mri_pruitt.txt" "${LOCAL_MRI_DIR}/mri_madcbrain.txt" | wc -l

comm -12 "${LOCAL_MRI_DIR}/mri_pruitt.txt" "${LOCAL_MRI_DIR}/mri_madcbrain.txt"

# COHERENCE_WP2_additional

The numerator of the "prevalence" follows the original definition - including old and new patients/users;

The age is defined at the time of matching;

National stats of drug: https://www.norpd.no/Prevalens.aspx 

National stats of disease: https://www.fhi.no/en/op/hin/

(Dropbox says I need more space.)

I remove the country code in front of the person_id and change it to "integer". (I will mention it at the biweekly meeting.)

For running without age and sex subgroups, comment away anything with `j` and `k`:
`for ( i...{...}`
`for ( k...{...}`
`temp17 <- temp17[temp17$Age_group==j & temp17$Gender==k,]`
`file_name <- paste0(wave_name,"_",j,"_",k)`
and write `file_name <- wave_name`.


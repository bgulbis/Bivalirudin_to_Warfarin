# Bivalirudin Transition to Warfarin
Andrea Fetea, Brian Gulbis, Christie Hall  
`r format(Sys.Date(), "%B %d, %Y")`  

## Project Description

The objective of this study is to evaluate the effect of bivalirudin on the International Normalized Ratio (INR) in patients being bridged to warfarin. This is a retrospective, cohort study, conducted at Memorial Hermann-Texas Medical Center in Houston, Texas. 

The primary endpoint is the change in INR at the first therapeutic aPTT after bivalirudin initiation and prior to warfarin initiation. Secondary endpoints include the change in INR at 12- and 24-hours after bivalirudin initiation and just prior to bivalirudin cessation to > 4 hours after bivalirudin cessation, days of overlapping bivalirudin and warfarin therapy, percent of time anticoagulated or over- / under-anticoagulated, percent of time with a therapeutic, supratherapeutic, or subtherapeutic INR, and incidence of major and minor bleeding events or new thrombotic events during the transition or post-transition interval.

Patients who were discharged home from Memorial Hermann-Texas Medical Center between July 1, 2012 and June 30, 2015 were screened for inclusion. Patients were included if they were $\ge$ 18 years old and received primary bridging therapy with overlapping bivalirudin and warfarin for $\ge$ 72 hours without interuption.

Patients were excluded if they received any alternate anticoagulant during the transition interval, had active major bleeding within 48 hours prior to bivalirudin initiation, a history of acute hepatitis, liver cirrhosis, antiphospholipid antibody syndrome, or lupus anticoagulant syndrome, INR > 1.5 at the time of bivalirudin initiation, received $\ge$ 2.5mg of phytonadione within 5 days prior to bivalirudin initiation, received systemic fibrinolytics withing 48 hours prior to bivalirudin initiation, received medications which are major inducers of warfarin metabolism, or were pregnant. 

## Study Design and Data Processing

### Definitions

Clinical Classifications Software (CCS) for ICD-9-CM, revised July 23, 2014, is used to categorize ICD-9 diagnosis codes by disease state. For more about CCS, go to https://www.hcup-us.ahrq.gov/toolssoftware/ccs/ccs.jsp

When all ICD-9 codes contained within a CCS group were not appropriate, the individual ICD-9 codes used are listed. The identification of individual ICD-9 codes used came from previously published literature.

### Data Collection
Data was obtained from the electronic medical record via the Enterprise Data Warehouse (EDW) using queries created in SAP BusinessObjects Enterprise XI, version 12.1.0. Data was collected at two time points, the index hospitalization encounter and the hospital-based reevaluation encounter.

#### EDW Queries
The following queries were created:

1. Identify Patients
    a. Query Result Objects
        i. PowerInsight Encounter Id
        i. Encounter Type
        i. MPP (which generated order)
        i. Order Catalog Mnemonic
    a. Query Filters
        i. Person Location- Facility (Curr)
            * Memorial Hermann Hospital
            * Memorial Hermann Transitional Care Facility
            * Memorial Hermann Rehabilitation Unit
        i. Date Prompt- Discharge
        i. Order Catalog Mnemonic
            * bivalirudin
            * bivalirudin 100 mg in NS 100 mL (for HIT)
            * bivalirudin 250 mg in NS 250 mL (for HIT)
            * bivalirudin 250 mg in NS 250 mL IV (titrate)
            * bivalirudin 250 mg in NS 50 mL IV (titrate)
        i. Admit Date
            * $\ge$ 7/1/2012 12:00:00 AM
        i. Age- Years (Visit)
            * $\ge$ 18

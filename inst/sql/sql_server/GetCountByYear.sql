/************************************************************************
    Copyright 2018 Observational Health Data Sciences and Informatics

This file is part of TicagrelorVsClopidogrel

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
************************************************************************/
{DEFAULT @cdm_database_schema = "cdm"}
{DEFAULT @work_database_schema = "cdm"}
{DEFAULT @study_cohort_table = "cohort"}

select cohort_definition_id, year(cohort_start_date) as year, count(*) as cohort_count, count (distinct subject_id) as person_count, SUM (DATEDIFF(DAY, cohort_start_date, cohort_end_date)) as cohort_date_sum
FROM @work_database_schema.@study_cohort_table
group by cohort_definition_id, year(cohort_start_date);
# Analysis

The scripts contained in this folder replicate the analysis reported in the paper.  These files run using R or Python as indicated in the file extension.

* **1_generate_data_v3.R**: Generates the data used in the analysis.
* **2_translation_assessment_v3.R**: Replicates Figure 1 in the paper reporting the machine-translation quality scores.  
* **3_aug_sum_v1.R**: Generates the analysis of data augmentation and summarization.
* **4_VocabSize_Counts Plot - Figure 5.Rmd**: Creates Figure 5 in the paper and shows the differences in vocabulary sizes.
* **4_main_results_v3.R**: Replicates the results reported in Figures 2, 3, and 4 in the paper. These indicate the performance of ConfliBERT for the binary classification (Figure 2),  multi-class classification for the QuadClass (Figure 3), and the  binary classification of each QuadClass category (Figure 4).
* **5_Rarity Means Plot - Figure 6.Rmd**: Generates separate rarity means plots that were merged as Figure 6 in the paper. 
* **5_Semantic_Distance_v3.ipynb**: Generates the data for semantic distance.
* **5_trees_v2.R**: Replicates the dependency trees reported in Table 1. It also replicates the Dependency Distance Mean in Figure 13 and its difference in Figure 14 as reported in Appendix C.
* **6_regression_v2.R**: Replicates the regression analysis reported in Appendix D.
* **7_rems_v1.R**: Replicates the Model Fit Loss reported in Figure 7 in the paper, and its corresponding Root Mean Square Error (RMSE) graphs in Appendix E.
* **10_Violin Plots appendix _ Figures 10 - 12.Rmd**: Generates separate Noun, Lemma, and Word count plots, that were merged as Figures 10, 11, and 12 in Appendix H.


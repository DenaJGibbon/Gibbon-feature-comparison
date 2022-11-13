# Plots

cowplot::plot_grid(MFCCScatter,
                   BirdNetScatterMean,
                   VGGishScatter,
                   AcousticIndicesScatter,
                   nrow = 2)


# Next analysis: randomly remove subset of calls (perhaps 20%) then see how well can classify new with noise (or augmented)
cowplot::plot_grid(mfcc.noiseScatter,
                   BirdNetNoiseScatterMean)

Overview
========

This is a simple project to test Diagonal-Outer-Product (DOP) lasyer as a substitute
for fully connected layer in neural networks. The dataset is HIGGS dataset https://archive.ics.uci.edu/ml/datasets/HIGGS#

The idea is to approximate affine layer ```Ax+b``` with the ```(diag(D)+uv^T)x+b```.


Training algorithm
===================

I am not a big fan of stochastic gradient descent so here I tried to implement conjugate gradient descent algorithm. Results indicate I succeed, more or less. It was fun - getting NaNs and everything.


Results
=========

Single logistic regression layer is trained to 64.1% accuracy on the 28 features, as in my previous attempt. I think it shows that CGD is implemented more or less properly.

Neural network of 12 layers of DOP plus logistic layer currently shows 65.9% accuracy which is comparable to 66.1% accuracy shown with logistic regression trained with reweited iterated least squares on extended (with simple squares) feature vector with 56 features.

Thus, I would like to suggest this fun abbreviated Diagonal-Outer-Product layer in your work.

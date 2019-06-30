# STAT 542 Project 3: Lending Club

This repository contains my solution for Project 3 in STAT 542 Practical Statistical Learning at the University of Illinois.

The project uses data available from [this Kaggle dataset](https://www.kaggle.com/wordsforthewise/lending-club). A subset of the available columns were cleaned and provided for us in [loan_stat542.zip](./loan_stat542.zip).

## Objective

The goal is to build a model that will predict the chance of default for a loan.

## Introduction (from the course project page)

After a loan is issued by lendclub, the loan becomes "Current".

- The ideal scenario: lending club keeps receiving the monthly installment payment from the borrower, and eventually the loan is paid off and the loan status becomes "Fully Paid."
- Signs of trouble: loan is past due with 15 days (In Grace Period), late for 15-30 days, or late for 31-120 days.
- Once a loan is past due for more than 120 days, its status will be "Default" or "Charged-off". Lending Club explains the difference between these two [Here]. For this project, we will treat them the same. 

We focus on closed loans, i.e., loan status being one of the following:

- Class 1 (bad loans): 'Default' or 'Charged Off';
- Class 0 (good loans): 'Fully Paid'. 


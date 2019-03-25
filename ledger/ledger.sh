#!/bin/bash

LEDGERFILE=${HOME}/notes/money/personal.ledger

function budget {
    # Show the remaining balance in a given budget.
    BUDGET=$1
    ledger bal --current -f $LEDGERFILE budgets:$BUDGET
    }

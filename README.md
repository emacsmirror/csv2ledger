# csv2ledger #

An Emacs Lisp package for converting csv files to ledger-cli entries.

## Introduction ##

The purpose of this small library is to read bank transactions in a CSV file and convert them to ledger entries. When properly configured, it can convert a CSV file automatically into a ledger file, taking a best guess at the target account to which transactions should be booked. When no target account can be deduced, a fallback account will be used, which you can change afterwards, or you can let Emacs show you each transaction and ask for a target account.

## Setup ##

Several user options must be configured before you can successfully use `csv2ledger`. Several other options can be configured to streamline the experience, but are not necessary.

First, let's look at a typical ledger entry, just to get some terminology right:

```
2022-20-17 * Aldi Supermarket
    Expenses:Groceries
    Assets:Checkings Account         -€25.10
```

This could be a ledger entry created from a CSV file for the bank account that's behind the `Assets:Checkings Account` in this entry. I call this account the *base account*, just so that I can refer to it. The second account in this example, `Expenses:Groceries` is the *target* account, also mainly just to give it a name. Lastly, I label "Aldi Supermarket" the *title*. In the `ledger-cli` documentation, this is usually called the payee, but for transactions where I receive money, I'm the payee, but I don't put my own name there. Instead, I write the name of the person or organisation that pays me, so I prefer to use the term *title*.

With that out of the way, let us look at the options that we need. First, you should set `c2l-base-account` to the ledger account that is associated with the bank account for which you are converting the CSV file. This can be done in two ways: You can either set a single, default base account, which will then apply to any CSV file you wish to convert, or you can set a buffer-local base account, which only applies to the CSV file in the current buffer.

Setting a default base file can be done through Customize or in your `init.el` file. In the latter case, make sure to use `setq-default`, not `setq`, or use the `:custom` directive of `use-package`.

Setting a buffer-local value for `c2l-base-account` can be done through file-local or directory-local variables, but also with the command `M-x c2l-set-base-account`. Regardless of the method you use, the buffer-local value will override any default value you may have set, so it is possible to set a default base account and override it on a per-buffer basis if the need arises.

The second option you should set is `c2l-csv-columns`. This defines the columns found in the CSV file. The default value for this variable is the following:

```
(date valuation description sender payee amount)
```

This means that the first column contains the date, the second the valuation, etc. You should change this list to whatever is correct for the CSV files you want to read. In order to create a proper ledger entry, your file should at least contain `date`, `sender`, `payee` and `amount`. The `valuation` and `description` fields can be added to the entry if they exist, but if not, they are left out.

If there are columns in your CSV files that you do not need for the ledger entry, you can write and underscore for them. For example, this is the setting that I use for my CSV files:

```
(setq c2l-csv-columns '(date _ type description sender payee amount _))
```

I ignore the second field (which contains the valuation) and the last one, which contains the account balance. In addition to the standard fields there is also a `type` field, which does not become part of the ledger entry but which I use to determine the correct target account, as described below.

These are the two options you must set in order to process any CSV files. There are a number of other options, however, which can make your life a bit easier if you configure them properly.

The first of these is `c2l-account-holder`. This should be a string or regular expression that matches your name, whatever your bank puts in the CSV file when you receive money, i.e., when you are the payee. This is used by `csv2ledger` to determine what to use as the title of a transaction. If you are the payee, it uses the sender, otherwise it uses the payee.

Second, you should set `c2l-fallback-account`. This is the account used as the target account when `csv2ledger` cannot determine a target account. This can be, but does not have to be a true ledger account. You can set it to e.g., `"TODO"`, so that after converting a CSV file you can go through the resulting ledger entries and search for the ones where you still need to provide a target account.

If you do not set `c2l-fallback-account`, conversion of a CSV file will not be entirely automatic: each time `csv2ledger` cannot determine a target account itself (as described below), it will ask you for one. If you prefer this method of operation, leave `c2l-fallback-account` unset.

The option `c2l-accounts-file` can be set to the path of a ledger file containing account declarations. Although `ledger-cli` does not require this, it is good style to define your accounts in a ledger file. Doing so and pointing `c2l-account-file` to it means that whenever `csv2ledger` asks you for an account, it uses completion.

If you have set these options, you are basically good to go. `csv2ledger` will convert a CSV file to ledger entries without complaining. However, target accounts will not be set automatically, so you will either have a lot of correcting to do, or you are being pestered by Emacs at every transaction.

## Automatic target account recognition ##

The automatic target account recognition in `csv2ledger` is admittedly fairly simple, but it works well for me. Essentially, it just checks for the presence of certain strings in an entry's fields. Each search string is associated with a ledger account. The first string that is found provides the target account.

To set this up, you first need to create a TSV file containing match strings and ledger accounts:

```
aldi          Expenses:Groceries
lidl          Expenses:Groceries
restaurant    Expenses:Leisure:Restaurant
```

The first column contains the match strings, the second column the ledger account. There can be multiple match strings associated with one account, as shown in the example.

With this file set up, you should point the option `c2l-account-matchers-file` to it so that the matchers can be used to determine the target account.

By default, `csv2ledger` only checks the `payee` and `description` fields in the CSV file for matches, but this can be configured with the option `c2l-title-match-fields`. I personally set this option to the following value:

```
(setq c2l-title-match-fields '(description payee sender type))
```

This means that the `description` field is checked first, then the `payee`, then the `sender` and lastly the `type` field. The `type` field is not part of the default setup, but it is listed in the CSV files I get from my bank and it indicates whether the transaction was a bank transfer, a recurrent order, an ATM withdrawal, a card payment at a store, etc. I have a matcher that captures ATM withdrawals that sets the target account to `Assets:Cash`. 

Note that this is the *only* reason for including the `type` field in `c2l-csv-columns` above: I use its value to help determine the target account.


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

This means that the first column contains the date, the second the valuation, etc. You should change this list to whatever is correct for the CSV files you want to read. In order to create a proper ledger entry, your file should at least contain `date`, `sender`, `payee` and `amount`. The `valuation` and `description` fields are added to the entry if they exist, but if not, they are left out.

If there are columns in your CSV files that you do not need for the ledger entry, you can write and underscore for them. For example, this is the setting that I use for my CSV files:

```
(setq c2l-csv-columns '(date _ type description sender payee amount _))
```

I ignore the second field (which contains the valuation) and the last one, which contains the account balance. In addition to the standard fields there is also a `type` field, which does not become part of the ledger entry but which I use to determine the correct target account, as described below.

These are the two options you must set in order to process any CSV files. There are a number of other options, however, which can make your life a bit easier if you configure them properly.

The first of these is `c2l-account-holder`. This should be a string or regular expression that matches your name, whatever your bank puts in the CSV file when you receive money, i.e., when you are the payee. This is used by `csv2ledger` to determine what to use as the title of a transaction. If you are the payee, it uses the sender, otherwise it uses the payee.

Second, you may also want to set `c2l-fallback-account`. This is the account used as the target account when `csv2ledger` cannot determine a target account. This can be, but does not have to be a true ledger account. You can set it to e.g., `"Expenses:TODO"`, so that after converting a CSV file you can go through the resulting ledger entries and search for the ones where you still need to provide a target account.

If you do not set `c2l-fallback-account`, conversion of a CSV file will not be entirely automatic: each time `csv2ledger` cannot determine a target account itself (as described below), it will ask you for one. If you prefer this method of operation, leave `c2l-fallback-account` unset.

The option `c2l-accounts-file` can be set to the path of a ledger file containing account declarations. Although `ledger-cli` does not require this, it is good style to define your accounts in a ledger file. Doing so and pointing `c2l-account-file` to it means that whenever `csv2ledger` asks you for an account, it offers your accounts for completion, which saves typing and ensures that you don't make typos in your account names..

If you have set these options, you are basically good to go. `csv2ledger` will convert a CSV file to ledger entries without complaining. However, you will either have the same target account in each transaction, which is probably not what you want, or Emacs will ask you at every transaction which target account to use. To make this a bit less cumbersome, you can have Emacs try to recognise the target account automatically.


## Automatic target account recognition ##

The automatic target account recognition in `csv2ledger` is admittedly fairly simple, but it works well for me. Essentially, it just checks for the presence of certain strings in an entry's fields. Each search string is associated with a ledger account. The first string that is found provides the target account.

To set this up, you first need to create a TSV (tab-separated values) file containing match strings and ledger accounts:

```
aldi          Expenses:Groceries
lidl          Expenses:Groceries
restaurant    Expenses:Leisure:Restaurant
```

The first column contains the match strings, the second column the ledger account. There can be multiple match strings associated with one account, as shown in the example. With this file set up, you should point the option `c2l-account-matchers-file` to it so that the matchers can be used to determine the target account.

What happens is that Emacs looks at the data for a transaction and check if one of the matchers is present in it. This is simple substring matching: if the string `"lidl"` is in the transaction, the target account is set to Expenses:Groceries.

By default, `csv2ledger` only checks the `payee` and `description` fields in the CSV file for matches, but this can be configured with the option `c2l-title-match-fields`. I personally set this option to the following value:

```
(setq c2l-title-match-fields '(description payee sender type))
```

This means that the `description` field is checked first, then the `payee`, then the `sender` and lastly the `type` field. The `type` field is not part of the default setup, but it is listed in the CSV files I get from my bank and it indicates whether the transaction was a bank transfer, an ATM withdrawal, a card payment at a store, etc. I have a matcher that captures ATM withdrawals that sets the target account to `Assets:Cash`. 

Note that this is the *only* reason for including the `type` field in `c2l-csv-columns` above: I use its value to help determine the target account. As mentioned, the `type` field is not included in the ledger entry.

## Modifying field values ##

Depending on the format of your CSV file, it may also be necessary to set the variable `c2l-field-parse-functions`. This is a list mapping fields to functions that take the field's value and convert it to something else. For example, my CSV files provide the date in the format `DD.MM.YYYY`, but ledger expects them to be in the format `YYYY-MM-DD`. `csv2ledger` comes with a function that performs this conversion, `c2l-convert-little-endian-to-iso8601-date`. I therefore set `c2l-field-parse-functions` like this:

```
(setq c2l-field-parse-functions
      '((date . c2l-convert-little-endian-to-iso8601-date)))
```

I have a similar problem with the amount. In the CSV file, amounts are given as follows: `3.150,20 €` or `-240,71 €`. I need to remove the dots and replace the decimal comma with a decimal dot. Furthermore, in my ledger file, the commodity € comes before the amount, but after the minus sign.

Since this is a very particular conversion, there is no function for it included in `csv2ledger`, but if you face the same problem, you can use or adapt the following:

```
(defun c2l-convert-postbank-to-ledger-amount (amount)
  "Convert AMOUNT from the format found in Postbank CSV files to ledger format.
Specifically, this converts an amount of the form \"-3.150,20 €\"
to \"-€3150.20\"."
  (string-match "\\(-\\)?\\([[:digit:].]+\\),\\([[:digit:]]\\{2\\}\\)" amount)
  (let ((sign (or (match-string 1 amount) ""))
        (euros (string-replace "." "" (match-string 2 amount)))
        (cents (match-string 3 amount)))
    (concat sign "€" euros "." cents)))
```

You can then add this to `c2l-field-parse-functions`:

```
(setq c2l-field-parse-functions
      '((date . c2l-convert-little-endian-to-iso8601-date)
        (amount . c2l-convert-postbank-to-ledger-amount)))
```

A final variable you may want to set is `c2l-alignment-column`. This should most likely have the same value as `ledger-post-amount-alignment-column`, although `csv2ledger` currently assumes that `ledger-post-amount-alignment-at` is set to `:end` and that the commodity precedes the amount. If either is not true, alignment is probably not optimal.


## Doing the conversion ##

There are three commands to convert CSV lines to ledger entries: `c2l-csv-entry-as-kill` converts the entry point is on and puts the result in the kill ring. It also displays the entry in the echo area so you can see what it is doing.

The command `c2l-convert-region` and `c2l-convert-buffer` convert the entries in the region or the entire buffer and put the results in a buffer called `*Csv2Ledger Results*`. Each time you call one of these conversion functions, the buffer is cleared, so make sure to save the ledger entries somewhere. You can also simply rename the buffer, Emacs will create a new buffer named `*Csv2Ledger Results*` if it doesn't find an existing one.


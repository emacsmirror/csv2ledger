# csv2ledger #

An Emacs Lisp package for converting CSV files to [ledger-cli](https://www.ledger-cli.org/) entries.

## Introduction ##

The purpose of this small library is to read bank transactions in a CSV file and convert them to ledger entries. When properly configured, it can convert a CSV file automatically into a ledger file, taking a best guess at the target account to which each transaction should be booked. When no target account can be deduced, a fallback account will be used, which you can change afterwards, or you can let Emacs show you each transaction and ask for a target account.

`csv2ledger` creates ledger entries of the following form (although ultimately everything is configurable):

```
2022-20-17 Aldi Supermarket
    ; Desc: Referenz 9999999XXX999 ALDI SAGT DANKE
    Expenses:Groceries
    Assets:Checking                                          -€25.10
```

The description is optional, you can leave it out if you prefer. The format used for the amount is also configurable. By default, `csv2ledger` just copies the amount from the CSV file, but you can apply a conversion to it if you like.

For ease of reference, I will use the following terms to refer to the various parts of the entry. The account associated with the bank account for the CSV file is called the *base* account. In the example here, it is `Assets:Checking`. The other account, here `Expenses:Groceries`, is the *balancing account*, though I also refer to it as the *target account*. The real-life entity associated with the balancing account, here "Aldi Supermarket" is often called the payee, but since I generally put the sender there if I am the payee (i.e., for transactions where I *receive* money), I refer to it as the *title*.

Not indicated in the above example is the *effective* date (also called *posted*). This is the date that may follow the booking date, separated by an equal sign:

```
2022-20-17=2022-20-19 * Aldi Supermarket
    ; Desc: Referenz 9999999XXX999 ALDI SAGT DANKE
    Expenses:Groceries
    Assets:Checking                                          -€25.10
```

If you have this information in your CSV file, you can use it and add it to the entry. If such an effective date is found, the entry is also marked as cleared, i.e., an asterisk appears between the date and the title.


## Installation ##

For the moment, `csv2ledger` needs to be installed manually. Put the file `csv2ledger.el` in your load path, byte-compile it if you wish, and `require` it in your init file.

Make sure to install the dependencies as well: `csv-mode` from GNU ELPA, and `parse-csv` and `dash` from Melpa. Note that for `csv2ledger` to work properly, CSV files must be opened in buffers with `csv-mode` as the major mode. This should work automatically after installing `csv-mode`, but if you have issues, make sure to check this.

The advantage of `csv-mode` is that it will also handle CSV files that use semicolon or TAB as separator (even if they have a `.csv` suffix). The separator should be recognised automatically without any user intervention.


## Customisation ##

Several customisation options are present. The full list with a short explanation is presented here, but most of these options are discussed in more detail below.

- `c2l-accounts-file` (`nil`) — File pointing to the ledger account definitions, used for completion.
- `c2l-base-account` (`"Assets:Checking"`) — Account of the CSV file.
- `c2l-fallback-account` (`nil`) — Target account if one cannot be determined automatically.
- `c2l-account-holder` (`nil`) — Name of the account holder that may appear as payee or sender.
- `c2l-csv-columns` (`'()`) — List describing the columns in the CSV file.
- `c2l-transaction-modify-functions` (`'(c2l-create-title c2l-create-amount c2l-create-account)`) — Functions used to modify the transaction data.
- `c2l-field-modify-functions` (`nil`) — Functions used to modify field values.
- `c2l-entry-function` (`#'c2l-compose-entry`) — Function used to create the actual ledger transaction.
- `c2l-account-matchers-file` (`nil`) — File with substrings matching specific target accounts.
- `c2l-target-match-fields` (`'(payee description)`) — Fields in the transaction used to match against target accounts.
- `c2l-auto-cleared` (`nil`) — Whether to auto-clear transactions.
- `c2l-alignment-column` (`52`) — Column to which the amount is aligned.



## Setup ##

At the very least, you will need to set two user options: `c2l-base-account` and `c2l-csv-columns`. `c2l-base-account` is the account that represents the bank account in your ledger file.  By default, it is set to the value `"Assets:Checking"`.

`c2l-csv-columns` is a list of column labels representing the columns in your CSV file. Note that these column labels should **not** be set to the column headers in your CSV file. Rather, they should be symbols indicating to `csv2ledger` what type of data each column contains. The following symbols are meaningful to `csv2ledger`:

- `date`: booking date of the transaction
- `posted`: effective date of the transaction
- `description`: whatever the bank provides
- `payee`: the party receiving the payment
- `sender`: the initiator of the payment
- `amount`: the amount of the payment (positive or negative)
- `credit`: the amount received
- `debit`: the amount payed
- `_` (underscore): ignored column

In the default setup, all these fields (except the underscore, obviously) may be used to create the Ledger entry, though some of them are only used in specific circumstances. Note that `c2l-csv-columns` should contain entries for *all* columns in the CSV file. Columns that you do not use should therefore be indicated with an underscore.

The `payee` and `sender` columns never appear both. By default, `payee` is used as the title of the ledger entry and `sender` is ignored. If you set the option `c2l-account-holder` however, the `sender` will be used as the title for transactions in which you are the payee, i.e., when you receive money. If you do not have a `sender` field in your CSV files, you may simply leave it out. In that case, the `payee` will always be used as the title (at least in the default setup).

The `amount` field is intended for the CSV field that contains the amount of the transaction. If your CSV files have two separate columns for amounts credit and amounts debit, use the column names `credit` and `debit` instead. `csv2ledger` then checks for each transaction which one of those fields actually contains an amount and uses that to create the ledger entry. Note that in this case, it is assumed that the `debit` field contains a negative amount, i.e., that it has a minus sign. If that is not the case, you should make sure that it does, as discussed below.

The `description` and `posted` fields are entirely optional. If you have them and wish to include them in the ledger entry, add them to `c2l-csv-columns`. If you do not wish them included in the ledger entries, ignore them.

As an example example, this is my setting for `c2l-csv-columns` (keep in mind that the column labels are symbols, not strings):

```emacs-lisp
(setopt c2l-csv-columns '(date _ type description sender payee amount _))
```

The CSV files from my bank have an effective (posted) date in them as the second column, but it is almost always identical to the booking date and does not provide me with any useful information. Furthermore, they also have an additional final column with the account balance, which `csv2ledger` doesn't use. So I use an underscore for both these columns.

Note that I have a `type` field in this list, which is not in the list of  fields above. You can, in fact, add any field to `c2l-csv-columns` that you like. By default, `csv2ledger` does not do anything with such user-defined fields, but with some additional configuration, you can make use of them in several ways, as discussed below. In my CSV files, the column that I label`type` indicates whether the transaction is a bank transfer, an ATM withdrawal, a card payment at a store, etc. I use this information to capture ATM withdrawals. (Details below.)


## Running the conversion ##

With these options set up, it is possible to convert a CSV file. To do so, open the CSV file in Emacs and run `c2l-convert-buffer`. This command creates a new buffer named `*Csv2Ledger Results*` and puts all converted CSV transactions in it. If you do not wish to convert the entire buffer, you can also select a region and call `M-x c2l-convert-region` instead. Note that if a buffer with the name `"Csv2Ledger Results"` already exists, it is reused. That is, its contents is erased before the new entries are put in it.

There is also the command `c2l-csv-entry-as-kill`: this converts the single entry that point is on and places the result in the kill ring. It also displays the entry in the echo area so you can see what it is doing. This is an easy way of testing if your conversion settings are correct.


## Setting the target account ##

In order to convert an entry, `csv2ledger` needs to know which account to use as the balancing account. By default, `csv2ledger` simply asks the user for each entry which account to use. To make `csv2ledger` recognise the balancing account automatically, you need to set up a file with account matchers. If the entry matches one of the matchers in this file, the corresponding account is used as the target account.

If no target account is found this way, you will be asked for the target account in the minibuffer. If you have defined your accounts in your ledger file (or in a file imported in your ledger file), you can point the option `c2l-accounts-file` to it and Emacs will provide those accounts as completion candidates.

If you prefer not to be asked for each unrecognised transaction, you can set the option `c2l-fallback-account` to an account that will be used instead. Note that this does not have to be an actual account. Rather, it should be something that you can easily search for in the ledger file, because you will most likely need to adjust the relevant transactions afterwards.

The account matchers file is a simple TSV (tab-separated values) file that matches strings to accounts:

```
aldi          Expenses:Groceries
lidl          Expenses:Groceries
restaurant    Expenses:Leisure:Restaurant
```

Set the option `c2l-account-matchers-file` to point to this file. With the example matchers shown here, if the payee or description (or any other field you configure) of a transaction contains the string `"aldi"`, `Expenses:Groceries` is taken as the balancing account. There can be more than one matcher for one account: in the example, both `"aldi"` and `"lidl"` link to the account `Expenses:Groceries`.

The matchers are simple substrings, not regular expressions. I have not found the need to use regular expressions for account matching, and prefer the simplicity of not having to worry about the special meaning of certain characters in them. Internally, however, the matchers are turned into regular expressions and stored in the variable `c2l-matcher-regexps`. If you prefer to use regular expressions, you can set this variable yourself. Its value should be an alist mapping regular expressions to accounts:

```
(setq c2l-matcher-regexps '(("\\(?:aldi\\|lidl\\)" . "Expenses:Groceries")
                            ("\\(?:restaurant\\)" . "Expenses:Leasure:Restaurant")))
```

`c2l-matcher-regexps` is not a customisable option. If you set it to a value yourself though, `csv2ledger` will not overwrite it (and ignore the value of `c2l-account-matchers-file`). Just make sure that the value is set before calling any functions from `csv2ledger` (but after loading the library), and keep in mind that if you have multiple regexps matching a transaction, the first regexp that matches wins out.

Matching an account specifically means matching the values of the fields listed in `c2l-target-match-fields` against the regexps in `c2l-matcher-regexps`. The first regexp that matches wins. By default, `c2l-target-match-fields` only contains the `payee` and `description` fields, but you can add other fields to it as well. (In fact, I set it to the value `(description payee sender type)`.)

Two things are of note here: first, the order of this list determines the order in which the fields get checked. The default value is `(payee description)`, so the `payee` field is checked before `description`. I prefer for the `description` field to be checked first, because it tends to contain more information than the `payee` field, so in my setup, I put `description` first. Second, I added the `type` field to the list. As already mentioned, `csv2ledger` does not do anything with this field, but I include it in `c2l-csv-columns` and I use it here to match the target account. Specifically, I use it to capture ATM withdrawals and set the target account to `Assets:Cash`.

Note that if you wish, you can completely forego the account matching mechanism discussed here and write your own function to find a target account. This is explained in a bit more detail below.


## Modifying field values ##

Sometimes, it is useful or even necessary to modify the value of some field extracted from the CSV file, e.g., when dates or amounts do not appear in a format that Ledger can use. There are two ways that allow you to do so.

### Modifying individual fields ###

First, there is the option `c2l-field-modify-functions`. You can use this option to adjust the value of particular fields. As an example, my CSV files provide the date in the format `DD.MM.YYYY`, but ledger expects them to be in the format `YYYY-MM-DD`. To remedy this, `csv2ledger` comes with the function `c2l-convert-little-endian-to-iso8601-date` that takes a date in the format `DD.MM.YYYY` and converts it to `YYYY-MM-DD`. For convenience, it also accepts dates in the forms `DD-MM-YYYY` and `DD/MM/YYYY`.

`c2l-field-modify-functions` is an alist mapping field names to functions. Each function should take a string as its only argument and return a string. They are called with the field's value as argument and the return value replaces the original value in the transaction. So in order to transform the date as described, I set `c2l-field-modify-functions` as follows:

```emacs-lisp
(setopt c2l-field-modify-functions
  '((date . c2l-convert-little-endian-to-iso8601-date)))
```

I have a similar problem with the amount. In the CSV file, amounts are given as follows: `3.150,20 €` or `-240,71 €`. I need to remove the dots and replace the decimal comma with a decimal dot. Furthermore, in my ledger file, the commodity sign € comes before the amount, but after the minus sign.

Since this is a very particular conversion, there is no function for it included in `csv2ledger`, but if you face a similar problem, you can use or adapt the following:

```emacs-lisp
(defun c2l-convert-postbank-to-ledger-amount (amount)
  "Convert AMOUNT from the format found in Postbank CSV files to ledger format.
Specifically, this converts an amount of the form \"-3.150,20 €\"
to \"-€3150.20\"."
  (string-match "\\(-\\)?\\([[:digit:].]+\\)\\(?:,\\([[:digit:]]\\{2\\}\\)\\)?" amount)
  (let ((sign (or (match-string 1 amount) ""))
        (euros (string-replace "." "" (match-string 2 amount)))
        (cents (or (match-string 3 amount) "00")))
    (concat sign "€" euros "." cents)))
```

The setting for `c2l-field-modify-functions` then ends up like this:

```emacs-lisp
(setopt c2l-field-modify-functions
  '((date . c2l-convert-little-endian-to-iso8601-date)
    (amount . c2l-convert-postbank-to-ledger-amount)))
```

Another possible use of `c2l-field-modify-functions` is to make sure the value of the `debit` field is is a negative value. For example, if your CSV file lists amounts debit as `"€25.14"` instead of `"-€25.14"`, you can change this with the following setup:

```emacs-lisp
(setopt c2l-field-modify-functions
  '((debit . (lambda (amount) (concat "-" amount)))))
```


### Modifying the transaction ###

One potential disadvantage of the functions in `c2l-field-modify-functions` is that they only take the value of a single field as argument. This is insufficient if you want to modify a field value on the basis of some other field or fields in the transaction. If you need to make such a change to the transaction, you can set the option `c2l-transaction-modify-functions` to a list of functions that take the entire transaction as its argument and return a modified transaction.

The transaction is passed as an alist of field-value pairs. For example, for the ledger entry shown above, the transaction would be as follows:

```
((date . "2022-20-17")
 (description . "Referenz 9999999XXX999 ALDI SAGT DANKE")
 (sender . "account holder")
 (payee . "Aldi Supermarket")
 (amount . "-€25.10"))
```

Note that the functions in `c2l-field-modify-functions` are applied before `c2l-transaction-modify-functions`, which is why the values for `date` and `amount` already appear in their modified forms here.

Your function can make any change to the transaction you wish. The only requirement is that after all functions in `c2l-transaction-modify-functions` have been applied, the resulting transaction alist contains at least the fields `date`, `payee`, `amount` and `account`,  because `csv2ledger` needs them to construct the ledger entry.

Note that the functions in `c2l-transaction-modify-functions` are applied in the order in which they appear in the list. Each function is passed the return value of the previous one, so you can add a field to the transaction in one function and refer to it in a later function.

`csv2ledger` actually uses `c2l-transaction-modify-functions` itself to construct the ledger entry. Therefore, the default value of this option contains three functions: one to create the title of the entry, one to set the amount and one to select the target account. These are discussed here in turn.


### Setting the title ###

As explained above, by default, `cs2vledger` uses the payee for the title of the ledger entry. However, it will use the sender as the title instead if the user option `c2l-account-holder` matches the payee, provided that option is set.

The particular function that takes care of this is `c2l-create-title` and it is the first function in `c2l-transaction-modify-functions`. You can replace it with a custom function if you want to construct the title differently.


### Setting the amount ###

The second function in `c2l-transaction-modify-functions` is a function to create the amount, imaginatively called `c2l-create-amount`. This function does not do anything if there is an `amount` field in the transaction and its value looks like an amount. If this is not the case, it checks if there is a `credit` or `debit` field and sees if either of those have a value that looks like an amount. If either of them exists and has an amount, it is added to the transaction as the `amount` field.

If you need to replace this function with a custom function, note that it is important that an `amount` field is created in the transaction, because that is what is eventually used to construct the entry. If a transaction does not have an `amount` field after the functions in `c2l-transaction-modify-functions` have been applied, the ledger entry will lack an amount and will be invalid.

Another important point to note is that the amount in the `amount` field must be a negative amount if it is an amount debit, i.e., it must have a minus sign. If you have a separate `debit` column in your CSV files with amounts that are not negative, make sure to add a minus sign. The easiest way to do this is in `c2l-field-modify-functions`.


### Setting the target account (v. 2) ###

The third function in `c2l-transaction-modify-functions` is `c2l-create-account`. This is the function that checks the fields of the transaction against the account matchers, and if one is not found, uses `c2l-fallback-account` or asks the user. If you wish to use a different method to set the account, you can replace this function with a custom one. It needs to add an `account` field to the transaction, but there are no restrictions on how the account is determined.


### Creating the entry ###

After all modification functions have been called, the resulting transaction is passed to the function pointed to by the user option `c2l-entry-function` . The default value of this option is the function `c2l-compose-entry`, which creates entries in the form shown above. If that format does not suit your needs, you can use a custom function instead. It should take the transaction as an alist and return a string that can be inserted into a ledger buffer.


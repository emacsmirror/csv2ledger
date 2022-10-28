# csv2ledger #

An Emacs Lisp package for converting csv files to ledger-cli entries.

## Introduction ##

The purpose of this small library is to read bank transactions in a CSV file and convert them to ledger entries. When properly configured, it can convert a CSV file automatically into a ledger file, taking a best guess at the target account to which transactions should be booked. When no target account can be deduced, a fallback account will be used, which you can change afterwards, or you can let Emacs show you each transaction and ask for a target account.

`csv2ledger` creates ledger entries of the following form:

```
2022-20-17 Aldi Supermarket
    ; Desc: Referenz 9999999XXX999 ALDI SAGT DANKE
    Expenses:Groceries
    Assets:Checkings                                         -€25.10
```

The description is optional, you can leave it out if you prefer. The format used for the amount is also configurable. By default, `csv2ledger` just copies the amount from the CSV file, but you can apply a conversion to it if you like.

For ease of reference, I will use the following terms to refer to the various parts of the entry. The account associated with the bank account for the CSV file is called the *base* account. In the example here, it is `Assets:Checkings`. The other account, here `Expenses:Groceries`, is the *balancing account*, though I also refer to it as the *target account*. The real-life entity associated with the balancing account, here "Aldi Supermarket" is often called the payee, but since `csv2ledger` may also put the sender here, i.e., the person or organisation that initiated the transaction, I refer to it as the *title*.

Not indicated in the above example is the *effective* date (also called *posted*). This is the date that may follow the booking date, separated by an equal sign:

```
2022-20-17=2022-20-19 * Aldi Supermarket
    ; Desc: Referenz 9999999XXX999 ALDI SAGT DANKE
    Expenses:Groceries
    Assets:Checkings                                         -€25.10
```

If you have this information in your CSV file, you can use it and add it to the entry. If such an effective date is found, the entry is also marked as cleared, i.e., an asterisk appears between the date and the title.


## Setup ##

At the very least, you will need to set `c2l-base-account` and `c2l-csv-columns`. `c2l-base-account` is the account that represents your bank account in your ledger file.  By default, `c2l-base-account` is set to `Assets:Checking`. `c2l-csv-columns` is a list of column names representing the columns in your CSV file. The default value for this variable is the following:

```
(date posted description sender payee amount)
```

Note that the column names are symbols. In the default setup, all these fields appear in the Ledger entry, except for the fact that `sender` and `payee` never appear both. By default, `payee` is used as the title of the CSV file and `sender` is ignored. The CSV files that I receive from my bank, however, have my name in the payee field when I receive money. In such cases, I want the sender to appear as the title of the ledger entry.

`csv2ledger` makes this happen if you set the option `c2l-account-holder` to a regular expression matching your name, or whatever your bank puts in the payee field in transactions where you receive money. If the payee matches this regular expression, the value of the `sender` field is used as the title.

If, on the other hand, you do not have a `sender` field in your CSV files, you may simply leave it out. In that case, the `payee` will always be used as the title.

The default value of `c2l-csv-columns` assumes that the transaction amount always appears in the same column. This is not always the case, however: you may have separate columns for amount credit and amount debit. If this is the case, you can use the column names `credit` and `debit` instead of `amount`. If `csv2ledger` doesn't find and `amount` field in `c2l-csv-columns`, it assumes the `credit` and `debit` fields are present and uses those to construct the ledger entry. Basically, it checks which one of those fields contains something that looks like an amount and uses that value.

If you have columns in your CSV files that you wish to ignore, use an underscore for them. For example, I'm not interested in the effective (posted) date, and my CSV files contain an additional final column with the balance, which `csv2ledger` doesn't use. Therefore, I set `c2l-csv-columns` to the following value:

```
(setq c2l-csv-columns '(date _ type description sender payee amount _))
```

One more thing to note here: I have a `type` field in this list. In my CSV files, this field indicates whether the transaction is a bank transfer, an ATM withdrawal, a card payment at a store, etc. `csv2ledger` itself does not do anything with the `type` field, but it will happily extract the information in the column if you provide a name for it. Choose any name you like, just don't use a name that is meaningful to `csv2ledger`.

Even though `csv2ledger` does not do anything with the `type` field by default, there are ways to make use of such extra information, as discussed below.


## Running the conversion ##

With these options set up, it is possible to convert a CSV file. To do so, open the CSV file in Emacs and run `c2l-convert-buffer`. This command creates a new buffer named `*Csv2Ledger Results*` and puts all converted CSV transactions in it. If you do not wish to convert the entire buffer, you can also select a region and call `M-x c2l-convert-region` instead. Note that if a buffer with the name `"Csv2Ledger Results"` already exists, it is reused. That is, its contents is erased before the new entries are put in it.

There is also the command `c2l-csv-entry-as-kill`: this converts the single entry that point is on and places the result in the kill ring. It also displays the entry in the echo area so you can see what it is doing.


## Automatic account recognition ##

In order to convert an entry, `csv2ledger` needs to know which account to use as the balancing account. By default, `csv2ledger` simply asks the user for each entry which account to use. To make `csv2ledger` recognise the balancing account automatically, you need to set up a file with account matchers. This file is a TSV (tab-separated values) file that matches strings to accounts:

```
aldi          Expenses:Groceries
lidl          Expenses:Groceries
restaurant    Expenses:Leisure:Restaurant
```

Set the option `c2l-account-matchers-file` to point to this file. With this setup, if the payee or description (or any other field you configure) of a transaction contains the string `"aldi"`, `Expenses:Groceries` is taken as the balancing account. There can be more than one matcher for one account: in the example, both `"aldi"` and `"lidl"` link to the account `Expenses:Groceries`.

The matchers are simple substrings, not regular expressions. I have not found the need to use regular expressions for account matching, and prefer the simplicity of not having to worry about the special meaning of certain characters in them. But if you prefer, you can use regular expressions for account matching. To do this, set the variable `c2l-account-regexps` to an alist mapping regular expressions to accounts:

```
(("\\(?:aldi\\|lidl\\)" . "Expenses:Groceries")
 ("\\(?:restaurant\\)" . "Expenses:Leasure:Restaurant"))
```

`c2l-account-regexps` is not a customisable option, because normally the variable is set based on the contents of the account matchers file. If it is already set to a value the first time a conversion function is called though, `csv2ledger` will not overwrite it.

When `c2l-account-regexes` is compiled from the account matchers file, each account has only one entry in the alist, but this is not a requirement. You can have multiple regexes pointing to the same account. Note that if you have multiple regexes matching a transaction the first regex that matches wins out.

By default, only the `payee` and `description` fields are compared against the account matchers. This can be configured with the option `c2l-target-match-fields`. Its default value is `(payee description)`, but you can add other fields to it. In fact, I set it to the value `(description payee sender type)`.

Two things are of note here: first, the order of this list determines the order in which the fields get checked. I prefer for the `description` field to be checked first, because it tends to contain more information than the `payee` field. Second, I added the `type` field to the list. `csv2ledger` does not do anything with this field, but I included it in the list of fields to extract from the CSV file and I use it here to match the target account. Specifically, I use it to capture ATM withdrawals and set the target account to `Assets:Cash`.


## Modifying field values ##

If you wish or need to modify the values extracted from the CSV file in some way, there are several user options that allow you to do so.

### Modifying individual fields ###

First, there is the option `c2l-field-modify-functions`. This is an alist mapping field names to functions. Each function should take a string as its only argument and return a string. These functions are called with the field's value as argument and the return value is used to construct the entry.

As an example, my CSV files provide the date in the format `DD.MM.YYYY`, but ledger expects them to be in the format `YYYY-MM-DD`. To remedy this, `csv2ledger` comes with the function `c2l-convert-little-endian-to-iso8601-date` that takes a date in the format `DD.MM.YYYY` and converts it to `YYYY-MM-DD`. For convenience, it also accepts dates in the forms `DD-MM-YYYY` and `DD/MM/YYYY`.

To use this function to modify the `date` field, I set `c2l-field-parse-functions` like this:

```
(setq c2l-field-modify-functions
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

The setting for `c2l-field-parse-functions` then ends up like this:

```
(setq c2l-field-modify-functions
      '((date . c2l-convert-little-endian-to-iso8601-date)
        (amount . c2l-convert-postbank-to-ledger-amount)))
```

### Modifying the transaction ###

One potential disadvantage of the functions in `c2l-field-modify-functions` is that they only take the value of a single field as argument. This is insufficient if you want to modify a field value on the basis of the other fields in the transaction. If you need to make such a change to the transaction, you can set the option `c2l-transaction-modify-function` to a function that takes the entire transaction as its argument and returns a modified transaction.

The transaction will be passed as an alist of field-value pairs. For example, for the ledger entry shown above, the transaction would be something like this:

```
((date . "17.20.2022")
 (description . "Referenz 9999999XXX999 ALDI SAGT DANKE")
 (sender . "account holder")
 (payee . "Aldi Supermarket")
 (amount . "-25,10 €"))

```

Your function can make any change to the transaction you wish. The only requirement is that the modified transaction contains at least the fields `date`, `payee` and either `amount` or `debit` and `credit`,  because `csv2ledger` needs them to construct the ledger entry.

Note that in this transaction alist, the values for date and amount have not been modified by the functions if `c2l-field-modify-functions`. This is because `c2l-transaction-modify-function` is called first. The result of that function is passed to the functions in `c2l-field-modify-functions`. In principle, `c2l-transaction-modify-function` can do anything `c2l-field-modify-functions` can do, but the latter type  of function is conceptually simpler, which is why it's included here.


### Setting the title ###

You may also notice that the transaction alist does not contain a value for `title`. The `title` field is added to the transaction alist after the functions in `c2l-field-modify-functions` have been applied. The function that creates the title is configurable through the option `c2l-title-function.` Its default value is `c2l-payee-or-sender`, which returns the sender if the payee matches the value of `c2l-account-holder` and the payee otherwise. It makes sure to check that it does not return an empty string, however, so if either payee or sender is empty, the other field's value is returned, and if both are empty, it returns the string `"Unknown payee"`.

`c2l-title-function` should be set to a function that takes a transaction as an alist and returns the title of the transaction as a string. This means that if you want to use something other than the payee or the sender as the title, e.g., the description field, you can. Just write a short function that extracts the relevant value from the transaction and returns it.


### Setting the amount ###

As already mentioned above, a CSV file may contain the amount of a transaction in a single column, or it may use separate columns for amounts debit and amounts credit.

TODO


A final variable you may want to set is `c2l-alignment-column`. This should most likely have the same value as `ledger-post-amount-alignment-column`, although `csv2ledger` currently assumes that `ledger-post-amount-alignment-at` is set to `:end` and that the commodity precedes the amount. If either is not true, alignment is probably not optimal.



-------------------------------------------------------------------------------

c2l-accounts-file nil
c2l-base-account "Assets:Unknown"
c2l-fallback-account nil
c2l-account-holder nil
c2l-csv-columns '(date posted description sender payee amount)
c2l-transaction-modify-function #'identity
c2l-field-modify-functions nil
c2l-title-function
c2l-amount-function
c2l-account-matchers-file nil
c2l-target-match-fields '(payee description)
c2l-auto-cleared nil
c2l-alignment-column 52


-------------------------------------------------------------------------------

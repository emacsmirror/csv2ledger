# csv2ledger #

An Emacs Lisp package for converting CSV files to [ledger-cli](https://www.ledger-cli.org/) entries.

## Introduction ##

The purpose of this small library is to read bank transactions in a CSV file and convert them to ledger entries. When properly configured, it can convert a CSV file automatically into a ledger file, taking a best guess at the target account to which each transaction should be booked. When no target account can be deduced, a fallback account will be used, which you can change afterwards, or you can let Emacs show you each transaction and ask for a target account.

`csv2ledger` creates ledger entries of the following form (although ultimately everything is configurable):

```
2022-20-17 Aldi Supermarket
    ; Desc: Referenz 9999999XXX999 ALDI SAGT DANKE
    Expenses:Groceries
    Assets:Checkings                                         -€25.10
```

The description is optional, you can leave it out if you prefer. The format used for the amount is also configurable. By default, `csv2ledger` just copies the amount from the CSV file, but you can apply a conversion to it if you like.

For ease of reference, I will use the following terms to refer to the various parts of the entry. The account associated with the bank account for the CSV file is called the *base* account. In the example here, it is `Assets:Checkings`. The other account, here `Expenses:Groceries`, is the *balancing account*, though I also refer to it as the *target account*. The real-life entity associated with the balancing account, here "Aldi Supermarket" is often called the payee, but since I generally put the sender there if I am the payee, I refer to it as the *title*.

Not indicated in the above example is the *effective* date (also called *posted*). This is the date that may follow the booking date, separated by an equal sign:

```
2022-20-17=2022-20-19 * Aldi Supermarket
    ; Desc: Referenz 9999999XXX999 ALDI SAGT DANKE
    Expenses:Groceries
    Assets:Checkings                                         -€25.10
```

If you have this information in your CSV file, you can use it and add it to the entry. If such an effective date is found, the entry is also marked as cleared, i.e., an asterisk appears between the date and the title.


## Setup ##

At the very least, you will need to set two user options: `c2l-base-account` and `c2l-csv-columns`. `c2l-base-account` is the account that represents your bank account in your ledger file.  By default, it is set to `Assets:Checking`. `c2l-csv-columns` is a list of column names representing the columns in your CSV file. The following column names are meaningful to `csv2ledger:`

- `date`: booking date of the transaction
- `posted`: effective date of the transaction
- `description`: whatever the bank provides
- `payee`: the party receiving the payment
- `sender`: the initiator of the payment
- `amount`: the amount of the payment (positive or negative)
- `credit`: the amount received
- `debit`: the amount payed

Note that the column names are symbols. In the default setup, all these fields may be used to create the Ledger entry, though some of them are only used in specific circumstances.

First,  `payee` and `sender` never appear both. By default, `payee` is used as the title of the ledger entry and `sender` is ignored. If you set the option `c2l-account-holder` however, the `sender` will be used as the title for transactions in which you are the payee, i.e., when you receive money. If you do not have a `sender` field in your CSV files, you may simply leave it out. In that case, the `payee` will always be used as the title, at least in the default setup.

The `amount` field is indented for the CSV field that contains the amount of the transaction. If your CSV files have two separate columns for amounts credit and amounts debit, use the column names `credit` and `debit` instead. `csv2ledger` then checks for each transaction which one of those fields actually contains an amount and uses that to create the ledger entry. Note that in this case, it is assumed that the `debit` field contains a negative amount, i.e., that it has a minus sign. If that is not the case, you should make sure that it does, as discussed below.

The `description` and `posted` fields are entirely optional. If you have them and wish to include them in the ledger entry, add them to `c2l-csv-columns`. If you do not wish them included in the ledger entries, leave them out.

Note that `c2l-csv-columns` should contain a column name for each column in your CSV files. If there are columns that you wish to ignore, use an underscore for them. For example, the CSV files from my bank have an effective (posted) date in them as the second element, but it is almost always identical to the booking date and does not provide me with any useful information. Furthermore, they also have an additional final column with the balance, which `csv2ledger` doesn't use. Therefore, I set `c2l-csv-columns` to the following value:

```emacs-lisp
(setq c2l-csv-columns '(date _ type description sender payee amount _))
```

Note that I have a `type` field in this list. In my CSV files, this field indicates whether the transaction is a bank transfer, an ATM withdrawal, a card payment at a store, etc. By default, `csv2ledger` itself does not do anything with the `type` field, but with some additional configuration, you can make use of such extra information in several ways, as discussed below.


## Running the conversion ##

With these options set up, it is possible to convert a CSV file. To do so, open the CSV file in Emacs and run `c2l-convert-buffer`. This command creates a new buffer named `*Csv2Ledger Results*` and puts all converted CSV transactions in it. If you do not wish to convert the entire buffer, you can also select a region and call `M-x c2l-convert-region` instead. Note that if a buffer with the name `"Csv2Ledger Results"` already exists, it is reused. That is, its contents is erased before the new entries are put in it.

There is also the command `c2l-csv-entry-as-kill`: this converts the single entry that point is on and places the result in the kill ring. It also displays the entry in the echo area so you can see what it is doing.


## Automatic account recognition ##

In order to convert an entry, `csv2ledger` needs to know which account to use as the balancing account. By default, `csv2ledger` simply asks the user for each entry which account to use. To make `csv2ledger` recognise the balancing account automatically, you need to set up a file with account matchers. This is a simple TSV (tab-separated values) file that matches strings to accounts:

```
aldi          Expenses:Groceries
lidl          Expenses:Groceries
restaurant    Expenses:Leisure:Restaurant
```

Set the option `c2l-account-matchers-file` to point to this file. With the example matchers shown here, if the payee or description (or any other field you configure) of a transaction contains the string `"aldi"`, `Expenses:Groceries` is taken as the balancing account. There can be more than one matcher for one account: in the example, both `"aldi"` and `"lidl"` link to the account `Expenses:Groceries`.

The matchers are simple substrings, not regular expressions. I have not found the need to use regular expressions for account matching, and prefer the simplicity of not having to worry about the special meaning of certain characters in them. But if you prefer, you can use regular expressions for account matching. To do this, set the variable `c2l-account-regexps` to an alist mapping regular expressions to accounts:

```
(("\\(?:aldi\\|lidl\\)" . "Expenses:Groceries")
 ("\\(?:restaurant\\)" . "Expenses:Leasure:Restaurant"))
```

`c2l-account-regexps` is not a customisable option, because normally the variable is set based on the contents of the account matchers file. If you set it to a value yourself though, `csv2ledger` will not overwrite it. (Just make sure the value is set before calling any functions from `csv2ledger`.)

When `c2l-account-regexes` is compiled from the account matchers file, each account has only one entry in the alist, but this is not a requirement. You can have multiple regexes pointing to the same account. Note that if you have multiple regexes matching a transaction the first regex that matches wins out.

By default, only the `payee` and `description` fields are compared against the account matchers. This can be configured with the option `c2l-target-match-fields`. Its default value is `(payee description)`, but you can add other fields to it. In fact, I set it to the value `(description payee sender type)`.

Two things are of note here: first, the order of this list determines the order in which the fields get checked. I prefer for the `description` field to be checked first, because it tends to contain more information than the `payee` field. Second, I added the `type` field to the list. As already mentioned, `csv2ledger` does not do anything with this field, but I include it `c2l-csv-columns` and I use it here to match the target account. Specifically, I use it to capture ATM withdrawals and set the target account to `Assets:Cash`.

Note that there is another way to automatically recognise an account, which involves writing your own function to do the recognition. This option is discussed below.


## Modifying field values ##

If you wish or need to modify the values extracted from the CSV file in some way, there are two user options that allow you to do so.

### Modifying individual fields ###

First, there is the option `c2l-field-modify-functions`. If the value of certain fields in your CSV file differ from the way they should appear in the ledger entry, you can use this option to adjust the value. As an example, my CSV files provide the date in the format `DD.MM.YYYY`, but ledger expects them to be in the format `YYYY-MM-DD`. To remedy this, `csv2ledger` comes with the function `c2l-convert-little-endian-to-iso8601-date` that takes a date in the format `DD.MM.YYYY` and converts it to `YYYY-MM-DD`. For convenience, it also accepts dates in the forms `DD-MM-YYYY` and `DD/MM/YYYY`.

`c2l-field-modify-functions` is an alist mapping field names to functions. Each function should take a string as its only argument and return a string. They are called with the field's value as argument and the return value replaces the original value in the transaction. So in order to transform the date as described, I set `c2l-field-modify-functions` as follows:

```emacs-lisp
(setq c2l-field-modify-functions
      '((date . c2l-convert-little-endian-to-iso8601-date)))
```

I have a similar problem with the amount. In the CSV file, amounts are given as follows: `3.150,20 €` or `-240,71 €`. I need to remove the dots and replace the decimal comma with a decimal dot. Furthermore, in my ledger file, the commodity sign € comes before the amount, but after the minus sign.

Since this is a very particular conversion, there is no function for it included in `csv2ledger`, but if you face a similar problem, you can use or adapt the following:

```emacs-lisp
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

The setting for `c2l-field-modify-functions` then ends up like this:

```emacs-lisp
(setq c2l-field-modify-functions
      '((date . c2l-convert-little-endian-to-iso8601-date)
        (amount . c2l-convert-postbank-to-ledger-amount)))
```

Another possible use of `c2l-field-modify-functions` is to make sure the value of the `debit` field is is a negative value. For example, if your CSV file lists amounts debit as `"€25.14"` instead of `"-€25.14"`, you can change this with the following function:

```emacs-lisp
(defun c2l-make-amount-negative (amount)
  "Add a minus sign to AMOUNT to make it negative.
AMOUNT should be a string representing a transaction amount."
  (concat "-" amount))
```

And add it to `c2l-field-modify-functions` as follows:

```emacs-lisp
(setq c2l-field-modify-functions
      '((debit . c2l-make-amount-negative))
```


### Modifying the transaction ###

One potential disadvantage of the functions in `c2l-field-modify-functions` is that they only take the value of a single field as argument. This is insufficient if you want to modify a field value on the basis of the other fields in the transaction. If you need to make such a change to the transaction, you can set the option `c2l-transaction-modify-function` to a function that takes the entire transaction as its argument and returns a modified transaction.

The transaction will be passed as an alist of field-value pairs. For example, for the ledger entry shown above, the transaction would be something like this:

```
((date . "2022-20-17")
 (description . "Referenz 9999999XXX999 ALDI SAGT DANKE")
 (sender . "account holder")
 (payee . "Aldi Supermarket")
 (amount . "-€25.10"))
```

Note that the functions in `c2l-field-modify-functions` are applied before `c2l-transaction-modify-functions`, which is why the values for `date` and `amount` already appear in their modified forms here.

Your function can make any change to the transaction you wish. The only requirement is that after all functions in `c2l-transaction-modify-functions` have been applied, the resulting transaction alist contains at least the fields `date`, `payee` and `amount` and `account`,  because `csv2ledger` needs them to construct the ledger entry.

Note that the functions in `c2l-transaction-modify-functions` are applied in the order in which they appear in the list. Each function is passed the return value of the previous one, so you can add a field to the transaction in one function and refer to it in a later function.

The default value of `c2l-transaction-modify-functions` contains three functions: one to create the title of the entry, one to set the amount and one to select the target account. These are discussed here in turn.


### Setting the title ###

As explained above, by default, `cs2vledger` uses the payee for the title of the ledger entry. (In fact, the ledger documentation tends to use the term *payee* for what I call the title.) It will use the sender as the title if the user option `c2l-account-holder` matches the payee, provided that option is set.

The particular function that takes care of this is `c2l-create-title` and it is the first function in `c2l-transaction-modify-functions`. You can replace it with a custom function if you want to construct the title differently.


### Setting the amount ###

The second function in `c2l-transaction-modify-functions` is a function to create the amount, imaginatively called `c2l-create-amount`. This function does not do anything if there is an `amount` field in the transaction and its value looks like an amount. If this is not the case, it checks if there is a `credit` or `debit` field and sees if either of those have a value that looks like an amount. If either of them exists and has an amount, it is added to the transaction as the `amount` field.

If you need to replace this function with a custom function, note that it is important that it creates an `amount` field in the transaction and that its value is an amount, because that is what is eventually used to construct the entry. If a transaction does not have an `amount` field after the functions in `c2l-transaction-modify-functions` have been applied, the ledger entry will lack an amount and will be invalid.

Another important point to note is that the amount in the `amount` field must be a negative amount if it's an amount debit, i.e., it must have a minus sign. If you have a separate `debit` column in your CSV files with amounts that are not negative, make sure to add a minus sign. The easiest way to do this is in `c2l-field-modify-functions`.


### Setting the account (v. 2) ###

The third function in `c2l-transaction-modify-functions`is `c2l-create-account`. This is the function that checks the fields of the transaction against the account matchers, and if one is not found, uses `c2l-fallback-account` or asks the user. If you wish to use a different method to set the account, you can replace this function with a custom one. It needs to add an `account` field to the transaction, but there are no restrictions on how the account is determined.


### Creating the entry ###

After all modification functions have been called, the resulting transaction is passed to the function in `c2l-entry-function` . The default value of this option is the function `c2l-compose-entry`, which creates entries in the form shown above. If that format does not suit your needs, you can use a custom function instead. It should take the transaction as an alist and return a string that can be inserted into a ledger buffer.


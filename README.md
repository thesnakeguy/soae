# soae — State of the Antarctic Environment

> A collaborative R package for generating figures and analyses for the **State of the Antarctic Environment (SOAE)** report.

---

## 🧭 Getting Started

### 1. Clone the repository

Open RStudio and run:

```r
usethis::create_from_github("thesnakeguy/soae")
```

Or clone the repository manually and open the `.Rproj` file.

### 2. Install required tools

```r
install.packages(c(
  "devtools",
  "usethis",
  "roxygen2",
  "testthat"
))
```

---

## 📦 Working with the Package

### Load the package during development

> ⚠️ Do **not** use `library(soae)` while developing. Use this instead:

```r
devtools::load_all()
```

This makes all functions in the package available immediately.

---

## ✍️ Adding or Editing Functions

All functions must be added or edited directly in the `R/` subdirectory. Create a thematic `.R` script if one doesn't exist yet — `roxygen2::roxygenise()` will scan the `R/` folder and document all scripts automatically.

```
R/gfw.R          # all things Global Fisheries Watch (gfw)
R/antarctic_theme.R  # stores the shared ggplot theme
```

### Writing a function

Use the roxygen2 template below as a starting point:

```r
#' Short title of the function
#'
#' Longer description of what the function does.
#'
#' @param x Description of input
#' @return Description of output
#' @examples
#' my_function(1:10)
#' @export
my_function <- function(x) {
  mean(x, na.rm = TRUE)
}
```

---

## 📚 Updating Documentation

Whenever you add or change a function, run:

```r
roxygen2::roxygenize()
```

This will:

- Generate/update documentation in `man/`
- Update the `NAMESPACE` file automatically

> ⚠️ Never edit `NAMESPACE` manually.

---

## 📦 Managing Dependencies

If your code uses a new package, manually edit the `DESCRIPTION` file and add it under `Imports:`:

```
Imports:
  ggplot2,
  dplyr,
  sf
```

> Do not use helper functions to edit `DESCRIPTION`.

---

## 🔍 Checking the Package

Before pushing changes, run:

```r
devtools::check()
```

Aim for:

- ✅ 0 errors
- ✅ 0 warnings
- ✅ 0 notes (or minimal notes)

---

## 🌿 Git Workflow

```bash
# 1. Create a branch
git checkout -b feature/my-change

# 2. Stage and commit
git add .
git commit -m "Describe your change"

# 3. Push and open a Pull Request
git push origin feature/my-change
```

Then open a **Pull Request** on GitHub.

---

## 🔄 Typical Workflow

1. Pull the latest changes from `main`
2. Create a new branch
3. Edit or add functions in the appropriate `R/*.R` script
4. Update documentation:
   ```r
   roxygen2::roxygenize()
   ```
5. Check the package:
   ```r
   devtools::check()
   ```
6. Commit and push your branch
7. Open a Pull Request

---

## 🧪 Key Rules

| Rule | Detail |
|------|--------|
| All functions go in `R/` | Organised by theme (e.g. `gfw.R`, `oceanography.R` etc) |
| Run `roxygenize()` after every change | Keeps docs and `NAMESPACE` in sync |
| Run `check()` before merging | Aim for 0 errors, 0 warnings |
| Never edit `NAMESPACE` manually | Always let roxygen2 handle it |
| Edit `DESCRIPTION` manually for dependencies | No helper functions |
| Keep functions simple and modular | One concern per function |
| Use the shared `antarctic_theme` | Consistent look across all figures |
| Start a function with `download` if it pulls in external data | Consistent nomenclature |
| Reference the type of function in the name | eg: `plot_gfw_***` for gfw plotting functions |

---

## 📬 Questions

If you're unsure about anything, open an [issue](../../issues)

---

Happy coding 🚀

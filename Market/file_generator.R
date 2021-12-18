# Цикл создания директорий и файлов
work_dir <- "~/DEV/R_projects/Market"
try(
  for (i in 1:10) {
  dir.create(glue({"{work_dir}/Marketplace_{i}/"}))
  file.create(glue("{work_dir}/Marketplace_{i}/import.in"))
  file.create(glue("{work_dir}/Marketplace_{i}/export.out"))
})
try(
  for (i in 1:10) {
    dir.create(glue({"{work_dir}/Review/"}))
    file.create(glue("{work_dir}/Review/marketplace_{i}.in"))
    file.create(glue("{work_dir}/Review/marketplace_{i}.out"))
})
try(
  for (i in 1:10) {
    file.create(glue("{work_dir}/Marketplace_{i}/import_all.in"))
    file.create(glue("{work_dir}/Marketplace_{i}/export_all.out"))
})

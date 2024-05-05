plutil::ursula()
#sterle <- spatialize("Мыс Стерлегова",area="point",style="longlat")
#sterle
#glance(sterle,zoom=5,resetGrid=TRUE)
#q()
skarga <- spatialize("Сопочная Карга",area="point",style="longlat")
dikson <- spatialize("Диксон аэропорт",area="point",style="longlat")
spatial_data(skarga) <- data.frame(wmo=20871L,name="Соп. Карга")
spatial_data(dikson) <- data.frame(wmo=20674L,name="о. Диксон")
bundle <- spatial_bind(skarga,dikson)
bundle
spatial_write(bundle,"C:/tmp/karga.geojson")
# glance(bundle,resetGrid=TRUE)

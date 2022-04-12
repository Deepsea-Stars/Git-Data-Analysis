import requests
import pandas as pd
from bs4 import BeautifulSoup
import time


class GetAQI:

    def __init__(self):
        ua = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_8; en-us) AppleWebKit/534.50 (KHTML, like Gecko) Version/5.1 Safari/534.50"
        self.headers = {
            "User-Agent": ua
        }
        city_name = ['lanzhou', 'haerbin', 'wulumuqi', 'xian', 'tianjin',
                     'guangzhou', 'shanghai', 'chengdu', 'nanjing', 'hangzhou']
        year_date = ['2018', '2019', '2020', '2021', '2022']
        for city in city_name:
            self.flag = True
            self.city = city
            for year in year_date:
                self.get_link(city, year)

    # 获取 url
    def get_link(self, city, year):
        url = None
        if year == '2020':
            self.save = False
            for day in range(1, 3):
                url = f"http://www.tianqihoubao.com/aqi/{city}-{year}0{str(day)}.html"
                self.get_aqidata(url, self.headers, year)
        else:
            self.save = False
            for day in range(1, 4):
                url = f"http://www.tianqihoubao.com/aqi/{city}-{year}0{str(day)}.html"
                self.get_aqidata(url, self.headers, year)
        print(f"{year}年{city}城市的数据存储成功!")

    # 获取整个数据
    def get_aqidata(self, url,  headers, y):
        html_data = requests.get(url=url, headers=headers).text
        print("获取html文件成功!")
        soup = BeautifulSoup(html_data, 'lxml')
        for line in soup.select('.api_month_list > table > tr'):
            aqi_data = line.text.replace(" ", "").strip().split('\n')
            if aqi_data[0] == '日期':
                continue
            aqi_data.remove('')
            aqi_data.remove('\r')
            day_list = [data.strip('\r') for data in aqi_data]
            self.time_get_data(day_list)
        time.sleep(4)

    # 按时间获取
    def time_get_data(self, day_list):
        true_f = ['2018-01-31', '2019-01-20',
                  '2020-01-09', '2021-01-27', '2022-01-16']
        flase_f = ['2018-03-16', '2019-03-05',
                   '2020-02-22', '2021-03-12', '2022-03-01']
        if day_list[0] in true_f:
            self.save = True
        if day_list[0] in flase_f:
            self.save = False

        if self.save:
            all_list = [day_list]
            self.write_file(all_list)

    # 写入文件

    def write_file(self, all_list):
        coln = ["日期", '质量等级', 'AQI指数', 'AQI排名',
                'PM2.5', 'PM10', 'SO2', 'NO2', 'CO', 'O3']
        test = pd.DataFrame(columns=coln, data=all_list)
        if self.flag:
            test.to_csv(f'AQI\\{self.city}.csv',
                        mode='a', encoding='utf-8', index=False)
            self.flag = False
        else:
            test.to_csv(f'AQI\\{self.city}.csv',
                        mode='a', encoding='utf-8', index=False, header=False)


if __name__ == "__main__":
    GetAQI()

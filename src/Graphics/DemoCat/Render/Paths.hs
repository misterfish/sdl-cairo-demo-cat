module Graphics.DemoCat.Render.Paths ( paths ) where

import           System.Environment ( getExecutablePath )
import           System.FilePath.Posix ( dropFileName )
import           Text.Printf ( printf )
import           Data.List ( dropWhileEnd, dropWhile )
import           Data.Char ( isSpace )

paths :: [String]
paths = map trim paths' where
    paths' = [ paths1
             , paths2
             , paths3
             , paths4
             , paths5
             , paths6
             , paths7
             , paths8
             , paths9
             ]

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

paths1 = " \n\
         \ M 162.00,135.00 \n\
         \ C 166.30,146.25 174.44,152.60 184.00,159.28 \n\
         \   188.05,162.11 192.94,165.51 198.00,165.88 \n\
         \   202.60,166.22 209.18,163.93 214.00,163.08 \n\
         \   221.56,161.76 226.43,161.91 234.00,162.00 \n\
         \   245.77,162.15 250.65,168.23 260.00,167.94 \n\
         \   266.12,167.75 268.66,164.80 274.00,162.81 \n\
         \   274.00,162.81 286.00,159.00 286.00,159.00 \n\
         \   288.83,162.84 295.27,168.46 293.36,173.89 \n\
         \   291.68,178.68 286.67,176.84 280.01,181.11 \n\
         \   275.83,183.79 273.69,187.28 270.56,191.00 \n\
         \   263.84,198.98 259.78,200.63 261.43,212.00 \n\
         \   261.72,214.01 262.20,217.21 263.17,218.94 \n\
         \   265.72,223.50 268.46,221.79 273.00,226.00 \n\
         \   259.07,232.15 251.03,215.19 249.00,204.00 \n\
         \   249.00,204.00 239.87,218.01 239.87,218.01 \n\
         \   239.87,218.01 224.00,224.47 224.00,224.47 \n\
         \   224.00,224.47 215.00,219.34 215.00,219.34 \n\
         \   215.00,219.34 208.74,212.96 208.74,212.96 \n\
         \   208.74,212.96 208.00,201.00 208.00,201.00 \n\
         \   202.83,203.54 190.14,210.52 190.41,216.99 \n\
         \   190.54,220.02 193.52,222.96 195.00,226.00 \n\
         \   185.84,225.46 185.80,222.80 184.82,215.00 \n\
         \   184.82,215.00 183.62,209.00 183.62,209.00 \n\
         \   183.15,203.84 187.02,201.63 188.49,197.00 \n\
         \   189.61,192.53 187.28,190.03 188.49,185.00 \n\
         \   189.52,181.76 192.69,177.60 191.52,174.82 \n\
         \   189.81,170.76 181.99,169.49 178.00,167.01 \n\
         \   169.40,161.66 157.29,145.72 159.00,135.00 \n\
         \   159.00,135.00 162.00,135.00 162.00,135.00 Z \n\
         \ M 220.19,205.00 \n\
         \ C 218.54,206.90 215.93,209.93 217.15,212.63 \n\
         \   217.68,213.78 219.10,214.80 220.06,215.57 \n\
         \   224.65,219.26 226.39,220.53 231.98,217.80 \n\
         \   233.54,217.04 235.03,216.25 235.98,214.73 \n\
         \   237.18,212.79 237.97,200.83 238.00,198.00 \n\
         \   229.10,198.12 226.51,197.75 220.19,205.00 Z \n"

paths2 = " \n\
         \ M 133.00,162.11 \n\
         \ C 141.26,162.48 143.91,169.81 149.04,174.96 \n\
         \   149.04,174.96 159.00,183.58 159.00,183.58 \n\
         \   161.46,185.57 165.87,189.61 169.28,188.80 \n\
         \   173.03,187.92 176.02,182.84 178.46,180.09 \n\
         \   180.94,177.29 186.88,172.53 190.00,170.30 \n\
         \   190.00,170.30 205.00,161.91 205.00,161.91 \n\
         \   205.00,161.91 218.00,152.39 218.00,152.39 \n\
         \   218.00,152.39 229.00,146.96 229.00,146.96 \n\
         \   229.00,146.96 238.00,141.99 238.00,141.99 \n\
         \   238.00,141.99 256.94,139.00 256.94,139.00 \n\
         \   252.93,147.24 259.73,149.95 256.94,155.94 \n\
         \   253.58,162.91 246.07,156.19 241.72,161.31 \n\
         \   237.00,166.88 242.51,169.55 244.26,174.04 \n\
         \   245.29,176.68 245.00,182.94 245.00,186.00 \n\
         \   238.81,183.83 242.15,179.84 238.57,178.25 \n\
         \   236.46,177.31 233.50,179.29 233.68,182.04 \n\
         \   233.87,184.85 237.64,188.15 238.99,191.00 \n\
         \   241.34,195.97 238.38,206.03 235.00,210.00 \n\
         \   232.16,203.19 234.94,202.11 233.40,198.04 \n\
         \   232.33,195.21 229.46,193.50 227.17,191.69 \n\
         \   226.30,191.00 224.82,189.60 223.75,189.32 \n\
         \   220.86,188.56 216.87,194.07 214.99,196.00 \n\
         \   214.99,196.00 202.91,209.37 202.91,209.37 \n\
         \   197.73,213.40 194.40,212.21 191.00,219.00 \n\
         \   191.00,219.00 206.00,222.00 206.00,222.00 \n\
         \   204.22,224.66 203.76,224.43 201.07,226.02 \n\
         \   199.78,226.79 198.14,228.02 196.72,228.40 \n\
         \   189.83,230.21 177.48,223.62 177.49,216.00 \n\
         \   177.50,214.22 178.34,212.60 179.00,211.00 \n\
         \   171.42,206.62 174.65,203.61 169.78,199.70 \n\
         \   167.64,197.98 163.68,197.00 161.00,195.60 \n\
         \   158.18,194.12 153.51,190.61 151.00,188.54 \n\
         \   140.09,179.56 139.25,173.96 126.00,165.00 \n\
         \   128.42,162.61 129.44,161.95 133.00,162.11 Z \n"

paths3 = " \n\
         \ M 154.00,154.01 \n\
         \ C 163.20,157.26 167.18,161.85 173.42,169.00 \n\
         \   176.23,172.22 180.15,177.05 185.00,175.93 \n\
         \   188.75,175.06 192.60,170.67 195.17,167.96 \n\
         \   199.91,162.95 203.25,159.66 209.00,155.75 \n\
         \   216.03,150.96 233.89,138.91 241.00,136.60 \n\
         \   247.21,134.57 252.65,135.07 259.00,135.00 \n\
         \   259.00,135.00 274.00,134.00 274.00,134.00 \n\
         \   274.00,134.00 273.31,137.00 273.31,137.00 \n\
         \   273.31,137.00 276.00,151.00 276.00,151.00 \n\
         \   267.93,159.36 266.62,148.93 253.00,153.00 \n\
         \   253.00,153.00 253.00,155.00 253.00,155.00 \n\
         \   255.23,155.75 258.84,156.73 260.40,158.51 \n\
         \   264.28,162.92 260.08,167.95 256.00,170.00 \n\
         \   256.00,170.00 257.00,166.00 257.00,166.00 \n\
         \   253.28,168.90 252.76,169.61 248.00,169.00 \n\
         \   248.00,169.00 250.00,165.00 250.00,165.00 \n\
         \   239.76,166.71 239.40,173.02 232.91,180.00 \n\
         \   225.31,188.16 218.03,191.17 214.34,195.18 \n\
         \   210.38,199.48 209.84,204.45 207.30,207.91 \n\
         \   204.17,212.18 196.12,215.83 195.62,219.17 \n\
         \   194.89,224.08 203.26,225.00 206.00,228.84 \n\
         \   204.16,229.00 200.67,229.09 199.00,228.84 \n\
         \   195.70,227.90 188.90,222.99 187.60,219.83 \n\
         \   186.97,218.31 187.06,216.60 187.00,215.00 \n\
         \   187.00,215.00 177.00,217.00 177.00,217.00 \n\
         \   177.06,222.67 176.77,223.93 181.00,228.00 \n\
         \   181.00,228.00 181.00,230.00 181.00,230.00 \n\
         \   174.02,231.47 174.07,226.56 174.00,221.00 \n\
         \   173.96,217.68 173.62,211.88 175.02,209.00 \n\
         \   176.87,205.19 181.85,202.16 181.74,195.00 \n\
         \   181.58,184.63 177.65,185.97 172.39,179.90 \n\
         \   168.62,175.55 166.94,169.01 158.00,162.64 \n\
         \   152.08,158.42 146.47,158.37 140.00,156.00 \n\
         \   143.34,150.07 148.59,152.10 154.00,154.01 Z \n"

paths4 = " \n\
         \ M 178.67,178.00 \n\
         \ C 178.67,178.00 185.33,156.00 185.33,156.00 \n\
         \   185.33,156.00 186.44,143.00 186.44,143.00 \n\
         \   186.97,139.72 189.46,133.58 188.65,131.00 \n\
         \   188.13,129.31 182.26,121.73 180.81,120.00 \n\
         \   180.81,120.00 165.63,102.60 165.63,102.60 \n\
         \   163.37,100.81 161.06,101.17 159.70,100.01 \n\
         \   158.40,98.80 158.43,96.90 159.70,95.59 \n\
         \   163.11,91.85 169.02,95.27 171.72,98.06 \n\
         \   175.32,101.79 182.00,108.94 184.77,113.00 \n\
         \   186.72,115.85 189.56,120.69 193.42,120.93 \n\
         \   198.44,121.23 208.50,111.97 213.00,109.09 \n\
         \   222.14,103.24 239.09,96.05 250.00,96.00 \n\
         \   276.25,95.88 265.73,104.07 289.00,101.00 \n\
         \   288.39,102.37 287.73,103.44 287.59,105.00 \n\
         \   287.39,107.22 292.19,119.80 284.89,122.01 \n\
         \   282.57,122.71 273.88,119.11 271.00,118.42 \n\
         \   269.13,117.67 266.27,116.77 264.74,118.42 \n\
         \   262.73,121.02 266.41,123.27 268.02,124.42 \n\
         \   273.63,128.48 279.61,136.44 270.00,140.00 \n\
         \   270.00,140.00 268.60,135.14 268.60,135.14 \n\
         \   265.06,128.02 257.46,132.58 252.00,133.00 \n\
         \   252.00,133.00 254.00,128.00 254.00,128.00 \n\
         \   251.60,127.59 248.32,126.63 246.00,127.00 \n\
         \   240.71,127.86 226.87,138.62 222.00,142.09 \n\
         \   217.68,145.17 212.04,147.85 208.91,152.09 \n\
         \   205.75,156.39 204.05,162.27 201.63,167.00 \n\
         \   198.79,172.53 194.62,175.35 194.28,182.00 \n\
         \   193.99,187.76 197.33,188.37 199.00,193.00 \n\
         \   186.57,192.56 190.12,181.71 188.00,173.00 \n\
         \   181.63,178.96 181.64,182.90 186.00,190.00 \n\
         \   177.20,192.22 177.31,184.39 178.67,178.00 Z \n"

paths5 = " \n\
         \ M 164.22,58.00 \n\
         \ C 164.22,58.00 157.42,49.00 157.42,49.00 \n\
         \   157.42,49.00 151.16,40.06 151.16,40.06 \n\
         \   150.10,37.13 152.45,35.89 155.02,36.66 \n\
         \   158.80,37.79 161.89,43.02 164.31,46.00 \n\
         \   172.01,55.47 180.18,68.71 194.00,67.91 \n\
         \   194.00,67.91 215.00,65.04 215.00,65.04 \n\
         \   215.00,65.04 231.00,66.17 231.00,66.17 \n\
         \   231.00,66.17 253.00,70.20 253.00,70.20 \n\
         \   253.00,70.20 267.00,72.41 267.00,72.41 \n\
         \   271.43,73.16 279.02,75.10 283.00,74.82 \n\
         \   283.00,74.82 291.00,74.00 291.00,74.00 \n\
         \   291.00,74.00 286.00,80.00 286.00,80.00 \n\
         \   287.25,80.80 288.09,81.04 288.98,82.41 \n\
         \   290.19,84.26 292.93,94.63 285.96,96.45 \n\
         \   283.03,97.21 273.38,93.98 270.00,93.00 \n\
         \   270.00,93.00 268.48,102.00 268.48,102.00 \n\
         \   268.48,102.00 272.85,110.00 272.85,110.00 \n\
         \   272.85,110.00 276.26,118.00 276.26,118.00 \n\
         \   276.26,118.00 284.00,131.00 284.00,131.00 \n\
         \   277.68,131.78 276.39,127.22 271.03,122.86 \n\
         \   266.77,119.40 263.63,114.96 259.83,111.02 \n\
         \   259.83,111.02 253.04,104.84 253.04,104.84 \n\
         \   250.90,102.84 248.60,99.84 245.91,98.74 \n\
         \   243.70,97.83 239.46,97.98 237.00,98.00 \n\
         \   237.00,98.00 216.00,98.99 216.00,98.99 \n\
         \   210.09,99.13 202.23,98.88 197.00,102.01 \n\
         \   190.68,105.79 188.08,111.69 183.68,117.00 \n\
         \   180.87,120.38 176.92,123.66 175.66,128.00 \n\
         \   174.33,132.62 177.66,139.63 178.00,147.00 \n\
         \   172.64,145.34 166.08,138.80 165.62,133.00 \n\
         \   165.62,133.00 165.62,122.00 165.62,122.00 \n\
         \   165.73,120.27 165.35,118.75 165.62,117.00 \n\
         \   166.22,114.71 169.02,110.56 170.20,108.00 \n\
         \   170.20,108.00 174.46,96.00 174.46,96.00 \n\
         \   177.21,88.18 177.53,83.86 183.00,77.00 \n\
         \   171.34,69.20 171.89,68.72 164.22,58.00 Z \n"

paths6 = " \n\
         \ M 183.88,77.00 \n\
         \ C 186.03,82.00 189.02,86.20 193.01,89.90 \n\
         \   197.97,94.48 209.36,97.84 217.00,102.69 \n\
         \   217.00,102.69 236.00,116.01 236.00,116.01 \n\
         \   241.85,120.53 248.47,126.15 256.00,127.24 \n\
         \   258.11,127.55 272.80,125.97 275.00,125.37 \n\
         \   275.00,125.37 283.00,123.00 283.00,123.00 \n\
         \   282.50,124.41 281.33,126.90 281.58,128.34 \n\
         \   281.86,129.92 286.37,134.55 284.68,140.89 \n\
         \   282.72,148.23 273.34,140.41 269.17,147.23 \n\
         \   265.01,154.04 272.07,170.39 276.39,176.00 \n\
         \   279.63,180.21 281.39,180.52 283.00,186.00 \n\
         \   271.89,185.10 272.20,175.93 263.00,169.00 \n\
         \   263.00,169.00 264.15,181.00 264.15,181.00 \n\
         \   264.15,181.00 270.00,192.00 270.00,192.00 \n\
         \   262.96,190.86 261.67,186.90 258.76,181.00 \n\
         \   252.85,169.03 254.49,165.35 244.72,155.05 \n\
         \   239.88,149.95 234.11,148.98 228.00,145.69 \n\
         \   228.00,145.69 209.00,135.76 209.00,135.76 \n\
         \   205.29,133.39 200.84,129.25 197.00,127.88 \n\
         \   192.82,126.38 188.29,127.44 184.00,126.67 \n\
         \   177.12,125.44 174.65,120.74 171.09,121.53 \n\
         \   166.07,122.65 166.59,130.92 158.00,134.00 \n\
         \   157.82,128.67 160.66,125.74 163.00,121.00 \n\
         \   163.00,121.00 156.00,121.00 156.00,121.00 \n\
         \   157.96,117.02 162.57,109.69 167.00,108.41 \n\
         \   169.27,107.75 170.90,108.95 173.00,109.27 \n\
         \   178.19,110.07 186.50,103.39 192.00,101.00 \n\
         \   187.81,95.28 184.86,94.39 181.63,89.99 \n\
         \   176.87,83.48 176.29,77.24 174.08,70.00 \n\
         \   172.70,65.49 171.70,65.17 171.00,60.00 \n\
         \   181.06,63.00 180.37,68.85 183.88,77.00 Z \n"

paths7 = " \n\
         \ M 196.04,108.00 \n\
         \ C 197.77,111.98 207.19,127.42 209.90,131.00 \n\
         \   209.90,131.00 218.39,141.00 218.39,141.00 \n\
         \   218.39,141.00 230.92,157.00 230.92,157.00 \n\
         \   235.01,161.99 238.68,167.57 244.00,171.36 \n\
         \   246.65,173.26 262.61,179.33 266.00,180.20 \n\
         \   272.95,181.99 273.58,177.66 281.00,179.00 \n\
         \   280.45,180.08 279.64,181.16 279.61,182.43 \n\
         \   279.55,184.75 286.13,192.96 282.22,197.60 \n\
         \   279.19,201.20 268.24,198.15 264.09,200.21 \n\
         \   261.35,201.58 257.46,206.96 257.09,210.00 \n\
         \   256.56,214.32 261.27,223.79 265.04,226.07 \n\
         \   267.17,227.35 269.60,227.65 272.00,228.00 \n\
         \   269.48,231.76 263.97,231.87 260.17,229.69 \n\
         \   256.82,227.76 248.73,216.63 246.30,213.02 \n\
         \   244.66,210.57 242.66,207.48 239.14,208.66 \n\
         \   234.42,210.23 234.26,219.38 236.74,222.79 \n\
         \   238.34,225.00 241.64,226.63 244.00,228.00 \n\
         \   244.00,228.00 244.00,230.00 244.00,230.00 \n\
         \   231.64,232.82 229.16,218.10 228.17,209.00 \n\
         \   227.88,206.36 228.25,203.45 226.98,201.04 \n\
         \   224.50,196.37 218.35,195.18 213.44,189.90 \n\
         \   207.02,183.00 205.18,176.54 200.82,172.11 \n\
         \   195.33,166.53 192.30,169.26 189.56,164.77 \n\
         \   186.46,159.70 187.39,152.74 186.00,147.00 \n\
         \   180.29,151.55 176.97,156.20 169.00,155.00 \n\
         \   169.00,155.00 172.00,151.00 172.00,151.00 \n\
         \   172.00,151.00 170.00,149.00 170.00,149.00 \n\
         \   173.45,147.93 175.88,147.72 179.00,145.66 \n\
         \   181.35,144.11 185.70,140.06 188.00,139.30 \n\
         \   192.74,137.74 194.41,143.35 199.84,138.35 \n\
         \   200.55,137.69 201.39,136.79 201.66,135.83 \n\
         \   202.47,133.00 194.87,123.93 193.06,121.00 \n\
         \   187.86,112.55 189.13,111.70 187.68,103.00 \n\
         \   186.90,98.33 186.28,98.71 186.00,93.00 \n\
         \   193.88,95.37 193.17,101.39 196.04,108.00 Z \n"

paths8 = " \n\
         \ M 190.77,157.00 \n\
         \ C 190.77,157.00 187.43,147.00 187.43,147.00 \n\
         \   184.90,136.34 183.78,125.89 186.00,115.00 \n\
         \   187.14,109.43 187.67,105.82 193.00,103.00 \n\
         \   193.00,103.00 191.10,124.00 191.10,124.00 \n\
         \   190.85,129.71 193.43,144.06 196.27,148.94 \n\
         \   199.60,154.65 211.96,155.43 218.00,158.42 \n\
         \   229.48,164.12 242.82,177.65 251.00,180.30 \n\
         \   253.63,181.16 256.27,180.93 259.00,181.19 \n\
         \   259.00,181.19 264.00,181.19 264.00,181.19 \n\
         \   267.08,180.82 270.50,177.62 274.00,176.00 \n\
         \   274.00,176.00 273.00,180.00 273.00,180.00 \n\
         \   273.00,180.00 278.00,178.00 278.00,178.00 \n\
         \   278.00,178.00 274.00,183.00 274.00,183.00 \n\
         \   275.40,183.45 276.24,183.43 277.59,184.59 \n\
         \   278.73,185.92 282.83,195.07 277.59,197.66 \n\
         \   274.08,199.55 265.88,197.99 262.00,198.82 \n\
         \   257.96,199.68 253.83,203.95 250.00,205.95 \n\
         \   246.11,207.99 243.11,207.74 241.18,209.01 \n\
         \   237.04,211.74 239.94,220.78 243.23,223.26 \n\
         \   245.34,224.86 247.51,224.85 250.00,225.00 \n\
         \   250.00,225.00 250.00,228.00 250.00,228.00 \n\
         \   243.30,228.00 239.64,229.02 235.36,222.96 \n\
         \   233.02,219.64 232.45,217.96 232.00,214.00 \n\
         \   227.23,216.68 226.21,219.89 225.00,225.00 \n\
         \   225.00,225.00 229.00,225.00 229.00,225.00 \n\
         \   229.00,225.00 229.00,228.00 229.00,228.00 \n\
         \   229.00,228.00 221.00,228.00 221.00,228.00 \n\
         \   219.87,219.49 224.45,209.37 221.97,205.39 \n\
         \   220.24,202.61 213.05,200.04 210.00,198.36 \n\
         \   207.94,197.22 202.70,192.66 200.54,194.03 \n\
         \   198.48,195.33 200.72,198.60 201.21,200.00 \n\
         \   203.48,204.08 204.24,205.48 201.21,209.00 \n\
         \   201.21,209.00 190.00,189.00 190.00,189.00 \n\
         \   190.00,189.00 186.00,190.00 186.00,190.00 \n\
         \   186.35,187.73 186.99,185.29 186.68,183.00 \n\
         \   186.02,178.16 181.29,172.43 188.00,170.00 \n\
         \   188.23,161.03 191.18,161.02 190.77,157.00 Z \n"

paths9 = " \n\
         \ M 195.43,215.00 \n\
         \ C 194.06,213.14 192.37,211.18 191.59,209.00 \n\
         \   190.83,206.83 191.01,203.35 191.00,201.00 \n\
         \   190.93,188.42 185.53,189.04 181.36,179.00 \n\
         \   178.94,173.17 181.39,171.29 180.78,166.00 \n\
         \   180.25,161.46 178.19,159.83 178.01,152.00 \n\
         \   177.80,142.82 181.71,130.86 186.34,123.00 \n\
         \   189.66,117.38 190.12,114.80 197.00,114.00 \n\
         \   194.05,124.98 194.18,120.85 190.41,129.00 \n\
         \   190.41,129.00 185.20,149.00 185.20,149.00 \n\
         \   184.85,152.64 185.72,158.76 190.11,159.52 \n\
         \   191.88,159.82 207.92,157.17 218.00,161.21 \n\
         \   223.81,163.53 231.05,168.07 236.00,171.89 \n\
         \   239.39,174.51 243.10,178.33 247.00,179.85 \n\
         \   253.27,182.31 263.74,181.03 270.00,179.00 \n\
         \   270.00,179.00 273.00,180.00 273.00,180.00 \n\
         \   273.00,180.00 270.00,183.00 270.00,183.00 \n\
         \   271.49,185.10 271.94,185.47 272.79,188.00 \n\
         \   272.79,188.00 274.08,193.00 274.08,193.00 \n\
         \   275.98,204.04 259.30,198.65 254.00,199.48 \n\
         \   249.30,200.21 245.77,204.38 241.00,205.59 \n\
         \   237.14,206.56 231.85,205.09 229.01,212.01 \n\
         \   228.39,213.52 228.04,215.38 228.36,217.00 \n\
         \   229.32,221.92 233.19,221.59 236.00,226.00 \n\
         \   227.90,226.99 228.80,226.47 222.00,222.00 \n\
         \   222.00,222.00 221.00,223.00 221.00,223.00 \n\
         \   221.00,223.00 223.00,228.00 223.00,228.00 \n\
         \   212.34,226.17 219.93,218.30 220.25,214.96 \n\
         \   220.62,211.14 218.10,204.36 214.87,202.21 \n\
         \   212.97,200.95 209.44,199.36 207.17,200.23 \n\
         \   204.78,201.15 200.19,206.08 201.94,208.78 \n\
         \   204.13,212.17 207.36,210.44 210.00,218.00 \n\
         \   210.00,218.00 202.00,215.00 202.00,215.00 \n\
         \   202.00,215.00 210.00,225.00 210.00,225.00 \n\
         \   202.10,224.16 199.90,221.11 195.43,215.00 Z \n"

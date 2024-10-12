'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation'
import Header from '../components/Header';

const Login = () => {
  const [email, setEmail] = useState('');
  const [password, setPassword] = useState('');
  const [error, setError] = useState('');
  const [isLoggedIn, setIsLoggedIn] = useState(false);
  const router = useRouter()

  useEffect (() => {
    if(isLoggedIn) {
      router.push('/')
    }
  })

  const handleSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();

    try {
      const response = await fetch('http://localhost:8085/api/v1/user/auth/login', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          emailForLogin: email,
          passwordForLogin: password,
        }),
      });

      if (response.status === 400) {
        const res = await response.text()
        setError(res || 'Invalid login credentials');
      } else if (response.ok) {
        const data = await response.json();
        localStorage.setItem('jwt_token', data.jwtToken);
        router.push('/')
      }
    } catch (error) {
      setError('An error occurred while logging in' + error);
    }
  };

  const handleGoogleLogin = () => {
    window.location.href = 'http://localhost:8085/api/v1/user/oauth2/login';
  };

  return (
    <div className="flex flex-col min-h-screen bg-white">
      <Header setIsLoggedIn={setIsLoggedIn} />
      <main className="container mx-auto mt-20 px-6 flex-grow">
        <h1 className="text-2xl font-bold mb-4 text-center">Login</h1>
        {error && <p className="text-red-500 text-sm mb-4">{error}</p>}
        <div className="card-bg px-6 py-6 shadow-lg rounded-lg mb-6 overflow-hidden">

        <form onSubmit={handleSubmit}>
          <div className="mb-4">
            <label htmlFor="email" className="block text-gray-700">Email</label>
            <input
              type="email"
              id="email"
              value={email}
              onChange={(e) => setEmail(e.target.value)}
              className="w-full px-3 py-2 border rounded"
              required
            />
          </div>
          <div className="mb-4">
            <label htmlFor="password" className="block text-gray-700">Password</label>
            <input
              type="password"
              id="password"
              value={password}
              onChange={(e) => setPassword(e.target.value)}
              className="w-full px-3 py-2 border rounded"
              required
            />
          </div>
          <button
            type="submit"
            className="w-full bg-blue-500 text-white py-2 rounded hover:bg-blue-600"
          >
            Login
          </button>
        </form>
        <div className="mt-4">
          <button
            onClick={handleGoogleLogin}
            className="w-full bg-red-600 text-white py-2 rounded hover:bg-red-700"
          >
            Continue with Google
          </button>
        </div>
        </div>
      </main>
    </div>
  );
};

export default Login;